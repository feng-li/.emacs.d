# Thunderbird star synchronization

Run `M-x notmuch-custom-sync-thunderbird-starred` to mirror Thunderbird's
cached stars into notmuch's `imap-starred` tag. The existing
`M-x notmuch-custom-sync-imap-starred` instead reads the IMAP server.
Both run in the background and refresh open notmuch buffers when finished.
Automatic indexing in Emacs (`notmuch new`, every five minutes, or manual `G`)
also runs the local summary scan after indexing succeeds. IMAP sync stays
manual. A failed local scan preserves existing stars, reports the error, and
still refreshes newly indexed mail. Running `notmuch new` directly in a shell
does not invoke this Emacs integration.

The local command uses `.msf` files alongside indexed Maildir folders under
`notmuch-custom-imap-post-send-accounts`' `:local-root` paths. Physical nested
`.sbd` paths are retained. It requires Python 3 and `tqdm`, selected through
`notmuch-custom-msf-python-command` (default `python3`). No network login is
needed. Progress and errors appear in `*notmuch-sync*`.

The Mork reader applies dictionary and row changes in file order, honors table
membership/removal and row/table replacement, and validates transaction
boundaries. Missing, changing, malformed, unsupported, aborted, or incomplete
summaries abort the scan before tagging. Virtual folders and summaries marked
for rebuilding are rejected. Thunderbird's files are read only.

A starred copy wins across folders. A tag is removed only when the Message-ID
is explicitly present and unstarred in the snapshot. Absent or deleted rows,
messages outside that snapshot, and the separate local `flagged` tag are left
alone. This conservative behavior means some stale stars may remain after a
message is deleted or moved out of the scanned folders; use the IMAP command
for server reconciliation. Thunderbird's on-disk summaries may lag its
in-memory state or the server, even when the files are stable.

The helper itself never changes tags. For a read-only scan, send it a JSON
array of full `.msf` paths on stdin, for example:

```sh
printf '["/path/to/Inbox.msf"]' | python3 site-lisp/notmuch-msf.py --summary
```

Parser regression tests:

```sh
python3 -m unittest discover -s site-lisp/tests -p 'test_notmuch_msf.py'
```

Emacs integration tests are in `site-lisp/tests/notmuch-msf-tests.el`; initialize
your package directory, add `site-lisp` to `load-path`, load that file, and run
`ert-run-tests-batch-and-exit`. Tests include an actual background worker with
an isolated fake notmuch executable, so they do not change the mail index.

Format references:
- [Mozilla Mork primer](https://www-archive.mozilla.org/mailnews/arch/mork/primer.txt)
- [Thunderbird Mork parser](https://github.com/mozilla/releases-comm-central/blob/master/mailnews/db/mork/morkParser.cpp)
- [Thunderbird Mork builder](https://github.com/mozilla/releases-comm-central/blob/master/mailnews/db/mork/morkBuilder.cpp)
