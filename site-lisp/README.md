# Local Emacs Lisp packages

This directory contains custom Emacs Lisp packages used directly by the main
configuration. `site-lisp/` is added to `load-path` recursively during startup,
so these packages do not need to be installed from ELPA.

## python-send-and-step

[`python-send-and-step.el`](python-send-and-step.el) provides standalone,
Elpy-independent Python evaluation commands. Its buffer-local minor mode binds
`C-c C-c` to send the current blank-line-separated group of top-level
statements and advance to the next group. Compound statements, decorators, and
blank lines inside a definition remain together. `C-c C-f` preserves the old
Elpy binding: it sends the containing definition, advances past it, and opens
the Python shell. `C-c C-n` sends the active region or current line and moves
down one line. `C-c C-r` sends the active region, or the whole buffer when no
region is active, and moves point to the end. A prefix argument permits
execution of an `if __name__ == "__main__"` block.

The main configuration enables `python-send-and-step-mode` in both
`python-mode` and `python-ts-mode` buffers.

## ivy-pinyin-search

[`ivy-pinyin-search.el`](ivy-pinyin-search.el) adds automatic Chinese-pinyin
matching to Ivy. In Swiper buffers and static Ivy collections containing
Chinese text, both initials such as `bj` and full pinyin such as `beijing` or
`bei jing` automatically match `北京`. Matching is case-insensitive, so `BJ`
works as well. Latin-only sources retain the ordinary Ivy matcher. A leading
backtick is unnecessary. Appending one, as in `` beijing` ``, forces pinyin
matching for Chinese candidates only, even in dynamic or otherwise undetectable
sources. Thus `` beijing` `` matches `北京` but not the Latin word `beijing`.
The package uses Emacs's built-in `chinese-py` input-method table and leaves any
command-specific Ivy regexp builders intact.

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
through `~/.virtualenvs/lsp/bin/mdict`, supplied by the Python `mdict-utils`
package. Configured StarDict sources are read through `sdcv`.
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

## company-numbered-selection

[`company-numbered-selection.el`](company-numbered-selection.el) turns the
unmodified keys `1` through `9` and `0` into direct selectors for Company's
first ten visible rows. If adding the pressed digit still matches a completion
candidate, the digit is inserted instead; this keeps identifiers such as
`sha256` typeable. In Company search mode the same decision uses the current
search expression and continues the search correctly.

The global `company-numbered-selection-mode` installs the bindings in both
`company-active-map` and `company-search-map`, displays matching quick-access
hints, and restores the previous bindings and hint settings when disabled. The
mode is enabled from `init.el` after Company loads.

Customize `company-numbered-selection-keys` to change the ordered selection
keys, or set `company-numbered-selection-show-hints` to nil before enabling the
mode to leave Company's hint display unchanged. Run
`M-x customize-group RET company-numbered-selection` for both options.

## yasnippet-personal-priority

[`yasnippet-personal-priority.el`](yasnippet-personal-priority.el) makes
personal snippets override non-personal snippets that use the same trigger
key. Yasnippet's normal directory order only replaces snippets with the same
identity, so differently named snippets from `yasnippet-snippets` can otherwise
remain alongside a personal definition.

The global `yasnippet-personal-priority-mode` applies the rule to direct
expansion, Yasnippet selection menus, and Company candidates supplied by
`company-yasnippet`. Multiple personal snippets sharing a trigger are retained,
as are bundled snippets whose triggers do not conflict. Optional Company
integration is attached when `company-yasnippet` loads and is removed cleanly
when the mode is disabled.

By default, files below `~/.emacs.d/snippets/` are considered personal.
Customize `yasnippet-personal-priority-directories` to recognize other snippet
trees. Template origins are held in a weak cache that is invalidated
automatically when this directory list changes; it can also be reset with
`M-x yasnippet-personal-priority-clear-cache`. The implementation advises
private Yasnippet and company-yasnippet candidate functions because no public
API exposes these lists at the required stage; keeping that dependency in this
package isolates the maintenance risk from `init.el`.
