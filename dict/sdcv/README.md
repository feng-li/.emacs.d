# Local dictionary data

This directory contains the local data used by
[`mdx-dict-and-synosaurus.el`](../../site-lisp/mdx-dict-and-synosaurus.el):

- MDX dictionaries queried with the `mdict` command from `mdict-utils`;
- StarDict dictionaries queried with `sdcv`; and
- optional cover images or source text kept with the dictionaries.

The configured Merriam-Webster Collegiate Dictionary and Thesaurus MDX file is
the primary source for dictionary browsing and Synosaurus. Soule's Dictionary
of English Synonyms is the Synosaurus fallback. The Merriam-Webster Advanced
Learner's Dictionary and Longman Dictionary of Common Errors are additional
StarDict sources shown by `mdx-dict-search`.

The configuration also contains a path for the optional Collins English
Dictionary and Thesaurus MDX file. A configured MDX source is skipped when its
file is absent or unreadable. An MDD companion file is not required for the
current text-only reader; embedded resources from such a file are not shown.

`setup_env.sh` installs `mdict-utils` in `~/.virtualenvs/lsp`. Install `sdcv`
separately and keep its dictionary basename metadata consistent with the names
configured in `init.el`. After replacing an MDX file, run
`M-x mdx-dict-clear-cache` so its headword and entry caches are rebuilt.

Some StarDict data originally came from the
[Free Dictionaries Project](http://download.huzheng.org/). Dictionary files
may have their own copyright and redistribution terms; verify those terms
before sharing them.
