#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Read Thunderbird Mork 1.4 summaries without modifying them.

Input: JSON array of .msf paths on stdin. Output: JSON starred/unstarred
Message-IDs on stdout. Diagnostics and tqdm progress go to stderr.
No notmuch writes occur here. Unsupported or inconsistent input fails closed.

Syntax and delta semantics checked against Mozilla's Mork primer and
mailnews/db/mork/{morkParser,morkBuilder}.cpp (MPL-2.0 reference source).
https://www-archive.mozilla.org/mailnews/arch/mork/primer.txt
"""

import argparse
import json
import re
import sys
from pathlib import Path

from tqdm import tqdm

MSG_SCOPE = "ns:msg:db:row:scope:msgs:all"
MSG_KIND = "ns:msg:db:table:kind:msgs"
FOLDER_KIND = "ns:msg:db:table:kind:dbfolderinfo"
HEX = re.compile(r"[0-9a-fA-F]+")
SPACE = re.compile(r"(?:\s+|\\\r?\n|//[^\r\n]*(?:\r?\n|$))+")


class InvalidMSF(ValueError):
    """A summary cannot safely provide flag information."""


class Mork:
    """Strict reader for the Mork constructs emitted in Thunderbird summaries.

Values are resolved when encountered, not against the final atom dictionary.
Rows are shared across tables; table membership and row replacement are
separate operations. Incomplete/aborted transactions reject the entire scan.
"""

    def __init__(self, data):
        self.s = data.decode("latin1")  # Preserve bytes until extracting IDs.
        self.i = 0
        self.atoms = {"a": {}, "c": {}}
        self.rows = {}
        self.tables = {}

    def fail(self, message):
        raise InvalidMSF(f"{message} at byte {self.i}")

    def skip(self):
        m = SPACE.match(self.s, self.i)
        if m:
            self.i = m.end()

    def peek(self):
        self.skip()
        return self.s[self.i:self.i + 1]

    def take(self, text):
        self.skip()
        if not self.s.startswith(text, self.i):
            self.fail(f"Expected {text!r}")
        self.i += len(text)

    def hex(self):
        self.skip()
        m = HEX.match(self.s, self.i)
        if not m:
            self.fail("Expected hexadecimal ID")
        self.i = m.end()
        return int(m[0], 16)

    def atom(self, scope, ident):
        if ident in self.atoms.get(scope, {}):
            return self.atoms[scope][ident]
        if scope == "c" and ident < 128:
            return chr(ident)
        self.fail("Undefined atom reference")

    def oid(self, default):
        ident = self.hex()
        scope = default
        if self.peek() == ":":
            self.take(":")
            if self.peek() == "^":
                self.take("^")
                scope = self.atom("c", self.hex())
            else:
                m = re.match(r"[A-Za-z_][A-Za-z_:?!+\-]*", self.s[self.i:])
                if not m:
                    self.fail("Unsupported scope")
                scope = m[0]
                self.i += len(scope)
        return scope, ident

    def value(self):
        """Read a literal including escapes, up to its unescaped closing )."""
        result = []
        while self.i < len(self.s):
            c = self.s[self.i]
            self.i += 1
            if c == ")":
                return "".join(result)
            if c == "\\":
                if self.i == len(self.s):
                    self.fail("Truncated escape")
                c = self.s[self.i]
                self.i += 1
                if c in "\r\n":
                    if c == "\r" and self.s[self.i:self.i + 1] == "\n":
                        self.i += 1
                    continue
            elif c == "$":
                digits = self.s[self.i:self.i + 2]
                if len(digits) != 2 or not HEX.fullmatch(digits):
                    self.fail("Invalid byte escape")
                c = chr(int(digits, 16))
                self.i += 2
            result.append(c)
        self.fail("Unterminated value")

    def cell(self, alias_scope=None):
        self.take("(")
        if alias_scope is not None:
            key = self.hex()
        elif self.peek() == "^":
            self.take("^")
            key = self.atom(*self.oid("c"))
        else:
            m = re.match(r"[A-Za-z_][A-Za-z_:?!+\-]*", self.s[self.i:])
            if not m:
                self.fail("Invalid column")
            key = m[0]
            self.i += len(key)
        c = self.peek()
        if c == "=":
            self.take("=")
            value = self.value()
        elif c == "^" and alias_scope is None:
            self.take("^")
            value = self.atom(*self.oid("a"))
            self.take(")")
        elif c == ")" and alias_scope is None:
            self.take(")")
            value = ""
        else:
            self.fail("Unsupported cell")
        return key, value

    def meta(self, end, scope=None):
        result = {}
        while self.peek() != end:
            if self.peek() == "(":
                k, v = self.cell()
                result[k] = v
            elif scope is not None and (
                self.peek() == "[" or bool(self.peek()) and self.peek() in "0123456789ABCDEFabcdef"
            ):
                self.row(scope)  # Metadata rows do not join table membership.
            else:
                self.fail("Unsupported metadata")
        self.take(end)
        # Different default atom/row scopes need context propagation; do not
        # silently interpret them using the standard Thunderbird scopes.
        for k in ("r", "rowScope", "a", "atomScope"):
            if k in result:
                self.fail("Nonstandard row/table scope metadata")
        return result

    def dictionary(self):
        self.take("<")
        scope = "a"
        if self.peek() == "<":
            self.take("<")
            metadata = {}
            while self.peek() == "(":
                k, v = self.cell()
                metadata[k] = v
            self.take(">")
            scope = metadata.get("a", metadata.get("atomScope", "a"))
            if scope not in ("a", "c"):
                self.fail("Unsupported dictionary scope")
        while self.peek() != ">":
            k, v = self.cell(alias_scope=scope)
            self.atoms[scope][k] = v
        self.take(">")

    def row(self, scope):
        bracket = self.peek() == "["
        clear = False
        if bracket:
            self.take("[")
            if self.peek() == "-":
                self.take("-")
                clear = True
        oid = self.oid(scope)
        row = self.rows.setdefault(oid, {})
        if clear:
            row.clear()
        if bracket:
            while self.peek() != "]":
                cut = self.peek() == "-"
                if cut:
                    self.take("-")
                if self.peek() == "[" and not cut:
                    self.take("[")
                    self.meta("]")
                    continue
                k, v = self.cell()
                if cut:
                    row.pop(k, None)
                else:
                    row[k] = v
            self.take("]")
        if self.peek() == "!":
            self.take("!")
            self.hex()  # Row order does not affect star state.
        return oid

    def table(self):
        self.take("{")
        clear = self.peek() == "-"
        if clear:
            self.take("-")
        oid = self.oid(None)
        if oid[0] is None:
            self.fail("Unscoped table")
        table = self.tables.setdefault(oid, {"members": set(), "kind": None})
        if clear:
            table["members"].clear()
        while self.peek() != "}":
            if self.peek() == "{":
                self.take("{")
                meta = self.meta("}", oid[0])
                kind = meta.get("k", meta.get("tableKind"))
                if kind:
                    table["kind"] = kind
            else:
                cut = self.peek() == "-"
                if cut:
                    self.take("-")
                row = self.row(oid[0])
                if cut:
                    table["members"].discard(row)
                else:
                    table["members"].add(row)
        self.take("}")

    def parse(self):
        if not self.s.startswith('// <!-- <mdb:mork:z v="1.4"/> -->'):
            self.fail("Not a Mork 1.4 summary")
        group = None
        while self.peek():
            c = self.peek()
            if self.s.startswith("@$${", self.i):
                if group is not None:
                    self.fail("Nested transaction")
                self.take("@$${")
                group = self.hex()
                self.take("{@")
            elif self.s.startswith("@$$}", self.i):
                self.take("@$$}")
                if group is None or self.hex() != group:
                    self.fail("Unmatched or aborted transaction")
                self.take("}@")
                group = None
            elif c == "<":
                self.dictionary()
            elif c == "{":
                self.table()
            elif c == "[":
                row = self.row(None)
                if row[0] is None:
                    self.fail("Unscoped standalone row")
            else:
                self.fail("Unsupported Mork construct")
        if group is not None:
            self.fail("Incomplete transaction")
        return self

    def flags(self):
        """Return known message flags, excluding deleted and detached rows."""
        members = set()
        folders = []
        for table in self.tables.values():
            if table["kind"] == MSG_KIND:
                members.update(table["members"])
            elif table["kind"] == FOLDER_KIND:
                folders.extend(self.rows[row] for row in table["members"])
        if len(folders) != 1:
            self.fail("Missing or ambiguous folder summary")
        info = folders[0]
        if info.get("forceReparse", "0") != "0" or info.get("version") != "1":
            self.fail("Folder summary needs rebuilding")
        # A virtual/saved-search folder is not an authoritative message table.
        if int(info.get("flags", "0"), 16) & 0x20:
            self.fail("Virtual folder summary")
        result = {}
        for oid in members:
            if oid[0] != MSG_SCOPE:
                self.fail("Wrong message row scope")
            row = self.rows[oid]
            raw_flags = row.get("flags", "")
            if not HEX.fullmatch(raw_flags):
                self.fail("Message has missing or invalid flags")
            flags = int(raw_flags, 16)
            if flags & (0x8 | 0x200000):  # Expunged or IMAP-deleted.
                continue
            mid = row.get("message-id", "").encode("latin1").decode("utf-8")
            if mid.startswith("<") and mid.endswith(">"):
                mid = mid[1:-1]
            if not mid or any(c.isspace() or ord(c) < 32 or c in "<>" for c in mid):
                self.fail("Message has missing or invalid Message-ID")
            result[mid] = result.get(mid, False) or bool(flags & 0x4)
        return result


def fingerprint(path):
    st = path.stat()
    return st.st_dev, st.st_ino, st.st_size, st.st_mtime_ns, st.st_ctime_ns


def scan(paths, progress=True):
    """Collect an all-or-nothing snapshot. Never infer unstarred from absence."""
    result = {}
    versions = {}
    paths = list(dict.fromkeys(Path(p) for p in paths))
    for path in tqdm(paths, desc="Thunderbird summaries", unit="folder", disable=not progress):
        before = fingerprint(path)
        data = path.read_bytes()
        if fingerprint(path) != before:
            raise InvalidMSF(f"Summary changed while reading: {path.name}")
        try:
            flags = Mork(data).parse().flags()
        except (ValueError, UnicodeError) as exc:
            raise InvalidMSF(f"{path.name}: {exc}") from exc
        versions[path] = before
        for mid, starred in flags.items():
            result[mid] = result.get(mid, False) or starred
    for path, before in versions.items():
        if fingerprint(path) != before:
            raise InvalidMSF(f"Summary changed during scan: {path.name}")
    return {"starred": sorted(mid for mid, value in result.items() if value),
            "unstarred": sorted(mid for mid, value in result.items() if not value),
            "folders": len(paths)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--summary", action="store_true", help="Print counts only (read-only check)")
    args = parser.parse_args()
    try:
        paths = json.load(sys.stdin)
        if not isinstance(paths, list) or not all(isinstance(p, str) for p in paths):
            raise InvalidMSF("Input must be an array of summary paths")
        result = scan(paths)
        if args.summary:
            result = {k: len(v) if isinstance(v, list) else v for k, v in result.items()}
        json.dump(result, sys.stdout, ensure_ascii=True)
        sys.stdout.write("\n")
    except (OSError, ValueError) as exc:
        print(f"MSF scan failed: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
