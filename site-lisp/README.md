# Local Emacs Lisp packages

This directory contains custom Emacs Lisp packages used directly by the main
configuration. `site-lisp/` is added to `load-path` recursively during startup,
so these packages do not need to be installed from ELPA.

## mdx-dict-and-synosaurus

[`mdx-dict-and-synosaurus.el`](mdx-dict-and-synosaurus.el) is a standalone
local dictionary and thesaurus package. It combines an MDX/StarDict browser
with the Synosaurus lookup, insertion, and replacement interface. The package
provides only the feature `mdx-dict-and-synosaurus`; the public Synosaurus
commands remain named `synosaurus-*`.

`mdx-dict-search` reads the active region or word at point and displays the
result immediately in `*MDict*`. It does not prompt for a word or dictionary.
The lookup prefers an exact or punctuation-insensitive MDX headword, then a
prefix match, and finally a nearby spelling. In the result buffer:

- `s` looks up the word at point;
- `b` and `f` move backward and forward through lookup history;
- `g` refreshes the current entry; and
- `RET` or mouse-1 follows an MDX cross-reference.

The main configuration binds `<f9> d` to `mdx-dict-search`. MDX files are read
through `/home/fli/.virtualenvs/lsp/bin/mdict`, supplied by the Python
`mdict-utils` package. Configured StarDict sources are read through `sdcv`.
Only readable MDX files are included, so an optional dictionary can be added or
removed without changing the package.

The Synosaurus backend first extracts thesaurus senses from the configured
Merriam-Webster Collegiate MDX file. If that entry contains no synonyms, it
falls back to Soule's Dictionary of English Synonyms through `sdcv`. The
minibuffer reports which source supplied each lookup. `<f9> s` runs
`synosaurus-choose-and-replace`; the other public commands are
`synosaurus-lookup` and `synosaurus-choose-and-insert`.

MDX headword indexes and queried entries are cached for the Emacs session. Run
`M-x mdx-dict-clear-cache` after replacing or modifying an MDX file. Use
`M-x customize-group RET mdx-dict` for dictionary paths and programs, and
`M-x customize-group RET synosaurus` for the chooser, backend, and prefix key.

## latexmkpvc

[`latexmkpvc.el`](latexmkpvc.el) adapts AUCTeX to the persistent process
created by `latexmk -pvc`. A normal AUCTeX command exits after one compilation;
`latexmk -pvc` remains alive, watches the document's inputs, and rebuilds
whenever one changes. The package registers this as the AUCTeX command
`LaTeXMkPvc`, using the following command template by default:

```text
latexmk -gg -pvc %(latexmk-out) %(file-line-error) %`%(extraopts) %S%(mode)%' %t
```

The integration makes a long-running build behave like a regular AUCTeX job:

- only output from the latest rebuild cycle is retained;
- diagnostics from the latest TeX-engine run are parsed by AUCTeX;
- a failed build opens the output below the source and, when appropriate,
  visits the first parseable source error;
- failures in auxiliary rules are shown at the relevant output position;
- an output window opened automatically for an error is hidden after a later
  successful build; and
- background TeX jobs do not take focus away from the active source buffer.

The main [`init.el`](../init.el) calls `latexmkpvc-setup`, enabling the mode in
every AUCTeX LaTeX buffer and making `LaTeXMkPvc` the default command. Press
`<f5>` to start the build. `C-c C-k` stops the current AUCTeX job and then
cleans intermediate and output files.

Run `M-x customize-group RET latexmkpvc` to change the command, whether output
is shown at startup or on failure, automatic error navigation, output cleanup,
or the `display-buffer` action. A persistent process inherits its environment
when it starts; restart the job after changing variables used by `latexmk` or
`.latexmkrc`.

## company-reftex

[`company-reftex.el`](company-reftex.el) supplies the
`company-reftex-citations` and `company-reftex-labels` Company backends. It
combines Company's completion UI with RefTeX's knowledge of a LaTeX document,
its bibliography, and its labels. Completion begins automatically inside
commands such as `\cite{...}`, `\textcite{...}`, `\ref{...}`, and
`\eqref{...}`.

Citation matching is broader than citation-key prefix matching: the typed text
is searched literally and case-insensitively across each raw BibTeX entry. This
makes it possible to find a reference by part of its key, author, title, year,
or another field. Candidate annotations are produced with RefTeX, while label
candidates include the label's associated content.

The citation backend is a cache-enabled redesign of the original
`company-reftex` package. It parses the relevant bibliography once, caches
matches for progressively longer prefixes, and reuses formatted candidates.
The cache is separated by bibliography source set and RefTeX sort settings. It
is invalidated automatically when a bibliography changes on disk or an open
BibTeX buffer has unsaved changes; file metadata are checked at most once per
second by default. Multi-file AUCTeX documents use their normal `TeX-master`
configuration and require no special cache setup.

The main configuration enables RefTeX in AUCTeX buffers and prepends both
backends to the buffer-local Company backend list. Useful controls are:

- `M-x company-reftex-clear-cache` to force the next citation completion to
  reparse its bibliography;
- `company-reftex-cache-validation-interval` to change how often source files
  are checked, with `0` meaning every candidate request;
- `company-reftex-annotate-citations` and
  `company-reftex-annotate-labels` to control annotations;
- `company-reftex-max-annotation-length` to truncate long annotations; and
- `company-reftex-citations-regexp` and `company-reftex-labels-regexp` to add or
  change the recognized LaTeX commands.

Run `M-x customize-group RET company-reftex` to edit these options through the
Customize interface.
