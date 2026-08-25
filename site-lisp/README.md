# Local LaTeX extensions

This directory contains two custom Emacs Lisp packages that provide the core
of this configuration's LaTeX workflow. `site-lisp/` is added to `load-path`
recursively during startup, so neither package needs to be installed from ELPA.

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
