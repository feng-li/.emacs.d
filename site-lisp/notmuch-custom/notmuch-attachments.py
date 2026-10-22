#!/usr/bin/env python3
"""Export one Notmuch thread without changing its messages, tags, or index.

Read a JSON job from stdin: message_query, directory, notmuch_command.
Use only `notmuch search` and `notmuch show`, with cached-key decryption only.
Write decoded attachment bytes and a collision-safe, append-only JSONL index.
"""

import json
import mimetypes
import os
from pathlib import Path
import re
import signal
import subprocess
import sys

try:
    from tqdm import tqdm
except ImportError:
    tqdm = None


def id_query(message_id):
    """Quote a Message-ID as a single Notmuch boolean term, not shell text."""
    return 'id:"' + message_id.replace('"', '""') + '"'


def safe_filename(name, fallback):
    """Discard path components and unsafe characters; bound UTF-8 length."""
    name = str(name or '').replace('\\', '/').rsplit('/', 1)[-1]
    name = re.sub(r'[<>:"|?*\x00-\x1f\x7f]', '_', name).strip(' .') or fallback
    stem, suffix = os.path.splitext(name)
    suffix = suffix.encode('utf-8')[:30].decode('utf-8', errors='ignore')
    stem = stem.encode('utf-8')[:200 - len(suffix.encode('utf-8'))].decode('utf-8', errors='ignore')
    return (stem or 'attachment') + suffix


def create_unique(directory, name):
    """Exclusively create a new file, never following or replacing a symlink."""
    stem, suffix = os.path.splitext(name)
    number = 1
    while True:
        candidate = directory / (name if number == 1 else f'{stem} ({number}){suffix}')
        try:
            fd = os.open(candidate, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
            return candidate, os.fdopen(fd, 'wb')
        except FileExistsError:
            number += 1


def read_json(command, *args):
    result = subprocess.run([command, *args], stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, check=True)
    return json.loads(result.stdout)


def messages_in(forest):
    """Walk all reply branches, including unmatched messages and missing parents."""
    pending = [node for thread in forest for node in thread][::-1]
    seen = set()
    while pending:
        message, replies = pending.pop()
        if isinstance(message, dict) and message.get('id') not in seen:
            if not message.get('id'):
                raise ValueError('Message metadata has no Message-ID')
            seen.add(message['id'])
            yield message
        pending.extend(reversed(replies or []))


def attachments_in(parts):
    """Yield file-like MIME parts, without exporting their nested parts twice."""
    pending = list(reversed(parts))
    while pending:
        part = pending.pop()
        if not isinstance(part, dict):
            continue
        if part.get('content-disposition') == 'attachment' or part.get('filename'):
            yield part
            continue
        # message/rfc822 content can contain a message object with its own body.
        content = part.get('content')
        if isinstance(content, list):
            pending.extend(reversed(content))
        body = part.get('body')
        if isinstance(body, list):
            pending.extend(reversed(body))


def error_text(error):
    if isinstance(error, subprocess.CalledProcessError):
        detail = (error.stderr or b'').decode('utf-8', errors='replace').strip()
        return f'notmuch exited {error.returncode}: {detail}'[:2000]
    return str(error)[:2000]


def extract(job, progress=True):
    """Resolve exactly one thread and export its attachments with provenance."""
    command = job.get('notmuch_command', 'notmuch')
    threads = read_json(command, 'search', '--format=json', '--output=threads',
                        '--exclude=false', job['message_query'])
    if not isinstance(threads, list) or len(threads) != 1:
        raise ValueError('The selected message must resolve to exactly one thread')
    if not isinstance(threads[0], str) or not re.fullmatch(r'[0-9a-fA-F]+', threads[0]):
        raise ValueError('Invalid thread identifier from notmuch')
    forest = read_json(command, 'show', '--format=json', '--format-version=5',
                       '--entire-thread=true', '--exclude=false', '--body=true',
                       '--decrypt=auto', 'thread:' + threads[0])
    messages = list(messages_in(forest))
    parts = [(message, part) for message in messages
             for part in attachments_in(message.get('body', []))]
    result = {'ok': True, 'messages': len(messages), 'saved': 0, 'errors': 0,
              'thread': threads[0], 'manifest': None}
    if not parts:
        print(f'No attachments found in {len(messages)} message(s).', flush=True)
        return result

    directory = Path(job['directory']).expanduser().absolute()
    directory.mkdir(parents=True, exist_ok=True)
    manifest, index = create_unique(directory, 'attachments.jsonl')
    result['manifest'] = str(manifest)
    print(f'Found {len(parts)} attachment(s) in {len(messages)} message(s).', flush=True)
    iterator = parts
    if tqdm is not None:
        iterator = tqdm(parts, desc='Attachments', unit='file',
                        disable=not progress or not sys.stderr.isatty())
    with index:
        for number, (message, part) in enumerate(iterator, 1):
            headers = message.get('headers', {})
            row = {'message_id': message['id'], 'date': headers.get('Date', ''),
                   'from': headers.get('From', ''), 'subject': headers.get('Subject', ''),
                   'part_id': part.get('id'), 'content_type': part.get('content-type', ''),
                   'original_filename': part.get('filename'), 'saved_filename': None}
            path = None
            try:
                part_id = part.get('id')
                if type(part_id) is not int or part_id < 1:
                    raise ValueError('Attachment has no valid MIME part number')
                extension = mimetypes.guess_extension(part.get('content-type', '')) or '.bin'
                name = safe_filename(part.get('filename'), f'attachment-{part_id}{extension}')
                path, output = create_unique(directory, name)
                try:
                    with output:
                        subprocess.run([command, 'show', '--format=raw',
                                        f'--part={part_id}', '--decrypt=auto',
                                        id_query(message['id'])], stdout=output,
                                       stderr=subprocess.PIPE, check=True)
                except BaseException:
                    # Only this newly created, incomplete export is removed.
                    path.unlink()
                    raise
                row.update(status='saved', saved_filename=path.name, bytes=path.stat().st_size)
                result['saved'] += 1
                print(f'[{number}/{len(parts)}] Saved {path.name}', flush=True)
            except (OSError, ValueError, subprocess.SubprocessError) as error:
                row.update(status='error', error=error_text(error))
                result['errors'] += 1
                print(f'[{number}/{len(parts)}] FAILED: {row["error"]}', flush=True)
            index.write((json.dumps(row, ensure_ascii=False) + '\n').encode('utf-8'))
            index.flush()
    result['ok'] = result['errors'] == 0
    print(f'Finished: {result["saved"]} saved, {result["errors"]} failed. Index: {manifest}', flush=True)
    return result


def main():
    try:
        result = extract(json.load(sys.stdin))
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        result = {'ok': False, 'error': error_text(error)}
    print('NOTMUCH-ATTACHMENTS-RESULT ' + json.dumps(result, ensure_ascii=False), flush=True)
    return 0 if result['ok'] else 1


def cancel_export(_signal, _frame):
    """Let per-file cleanup remove an incomplete export on cancellation."""
    raise KeyboardInterrupt


if __name__ == '__main__':
    signal.signal(signal.SIGTERM, cancel_export)
    raise SystemExit(main())
