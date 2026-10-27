# Notmuch customizations

This folder contains the Notmuch customization library, background helpers,
and regression tests. Emacs adds `site-lisp/` recursively to `load-path`, so
`(require 'notmuch-custom)` continues to work. The `tests/.nosearch` marker
keeps test libraries off the startup load path.

## Open and navigate threads

- **RET** or a click in a flat folder opens the complete thread with the
  selected message previewed. In ordinary search results it opens only the
  selected thread. Folder/search filters do not hide other replies.
- The thread list uses a **reverse reply tree**: replies above their parents,
  with branch grouping and indentation retained. Read upwards from a parent
  to follow its replies. This is a vertical mirror of the normal tree, not a
  strict date sort across separate branches. Repeated subjects are abbreviated
  upwards, keeping the topic visible on the bottom message of each subject group.
- **Up/Down** in the thread immediately previews the adjacent email, including
  non-matching replies. Flat folder lists retain ordinary cursor navigation.
- Inside the thread, **RET** keeps Notmuch's normal preview behavior; **q**
  returns to the previous view. Folder lists remain flat.

Set `notmuch-custom-tree-reverse-order` to `nil` and refresh with **=** to
restore the normal parent-first tree. Outline folding groups whole threads;
it does not fold individual reverse reply branches. The existing
`notmuch-custom-show-newest-first` option independently controls chronology
in full-message (`notmuch-show`) views.

## Export attachments from an opened thread

Open a thread, press **`C-c C-e`**, and choose a destination directory once.
Alternatively, run **`M-x notmuch-custom-extract-thread-attachments`**.
It works in the opened thread tree or its message preview, but not in flat
folder lists or ordinary search results.
The existing **`w`** command still saves the current message's attachments.

The exporter resolves the thread containing the message at point, rather
than exporting the entire search that opened it. It includes all indexed
messages in that thread, including collapsed, non-matching, and excluded
replies. Progress and errors appear in `*notmuch-attachments*`; Emacs remains
usable while the background worker runs. Only one export runs at a time.

- MIME transfer encoding is decoded without altering binary file bytes.
- Explicit attachments and named inline files are included, including named
  signature images. Unnamed inline text/HTML bodies are not exported.
- An attached `.eml` is saved as one file, without separately duplicating its
  nested attachments.
- Path components and unsafe filename characters are removed. Existing files
  and symlinks are never overwritten: duplicate names become `report (2).pdf`,
  `report (3).pdf`, and so on. Separate messages' attachments remain separate.
- Each run creates a new `attachments.jsonl` index (also collision-safe), with
  one JSON object per attachment: original/saved filename, Message-ID, date,
  sender, subject, MIME part number/type, byte count, and success/error status.
  The index is flushed after each file, and failed reads do not leave a newly
  created partial attachment behind. A failure does not stop other files.
- Mail files, tags, and the Notmuch index are not modified. Attachments are
  never opened automatically. A thread with no attachments creates no files.

The destination must be local; Python 3 and the existing Notmuch CLI are
required. `notmuch-custom-attachments-python-command` selects the Python
executable. No new Python dependency is required: optional `tqdm` gives a
terminal progress bar when available, while per-file progress is always
logged. Only locally available messages can be read. Encrypted content needs
a session key already cached by Notmuch (`--decrypt=auto`); the background
exporter does not prompt for keys, log in to IMAP, or fetch missing mail.

## Files

- `notmuch-custom.el`: saved searches, address completion, thread display,
  quote formatting, drafts/forwards, and background indexing/star sync.
- `notmuch-custom-thread-view.el`: thread opening, reverse trees, and previews.
- `notmuch-custom-attachments.el` and `notmuch-attachments.py`: thread export.
- `notmuch-msf.py`: read-only Thunderbird summary parser; see
  [star synchronization details](notmuch-msf.md).
- `tests/`: all Notmuch-specific regression tests formerly scattered across
  `site-lisp/` and `site-lisp/tests/`.

## Tests

From the `.emacs.d` repository root, run the Python tests:

```sh
python3 -m unittest discover -s site-lisp/notmuch-custom/tests -p 'test_notmuch_*.py' -v
```

The attachment integration tests create an isolated temporary mail directory
and Notmuch index; they never use your real mailbox or index. They verify
decoded binary bytes, complete thread scope, duplicate/unsafe filenames,
symlink and index-file collisions, and unchanged mail hashes/tags.

For Lisp tests, initialize your package directory as usual, add **both**
`site-lisp` and `site-lisp/notmuch-custom` to `load-path`, and load the desired
files from `tests/`. The attachment and quote checks can be run together:

```sh
emacs -Q --batch \
  --eval '(progn (require (quote package)) (package-initialize))' \
  -L site-lisp -L site-lisp/notmuch-custom \
  -l site-lisp/notmuch-custom/tests/notmuch-attachments-tests.el \
  -l site-lisp/notmuch-custom/tests/notmuch-custom-quote-test.el \
  --eval '(ert-run-tests-batch-and-exit "notmuch-\\(attachments-\\|custom-quote-\\)")'
```

If packages live in a version-specific directory, set `package-user-dir`
before `package-initialize`. The legacy IMAP/search/async test files are
preserved too; some older expectations predate the current search and local
star-sync implementation and are not part of the focused command above.
