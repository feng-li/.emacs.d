# Feng Li's Emacs configuration

This is a personal, batteries-included Emacs setup for research, technical
writing, and software development. It is centered on LaTeX/BibTeX, Python, and
R, with additional support for Julia, Scala, Markdown, Org, C/C++, Octave, and
YAML.

The configuration is intentionally opinionated. It changes several familiar
Emacs bindings, restores sessions automatically, starts Treemacs in interactive
sessions, and expects a number of command-line tools. Read the
[personalization](#personalization) and [key bindings](#selected-key-bindings)
sections before adopting it wholesale.

## Highlights

- Ivy, Counsel, and Swiper for minibuffer completion and search, including
  ripgrep, fzf, and optional Chinese-pinyin matching.
- Company and Yasnippet completion, with bundled snippets taking precedence
  over snippets with the same trigger from `yasnippet-snippets`.
- Projectile, Treemacs, Ibuffer, Magit, multiple cursors, and iedit for project
  and code navigation.
- AUCTeX, RefTeX, Company RefTeX completion, and a custom continuous
  `latexmk -pvc` workflow for LaTeX and BibTeX authoring.
- Elpy, LSP mode, pylsp, Flycheck, and common formatters/checkers for Python.
- ESS and poly-R for R and R Markdown, plus modes for Julia and Scala/Metals.
- Jinx, Hunspell, LanguageTool, Pandoc, and bundled StarDict dictionaries for
  prose editing.
- Dracula theme, adaptive visual wrapping, line numbers, tree-sitter mode
  remapping, desktop restoration, and host-specific state directories.
- gptel through a configured OpenAI-compatible backend, with credentials read
  through Emacs auth sources.

## Requirements

GNU Emacs 29 or newer is recommended. The setup relies on the built-in
`use-package` and tree-sitter APIs available in modern Emacs; it is currently
used with Emacs 31. Git and network access are needed for the initial clone and
first package installation.

Packages are installed from the Tsinghua GNU ELPA and MELPA mirrors configured
near the top of `init.el`. Official GNU ELPA and MELPA URLs are present there as
commented alternatives. Packages are not pinned, so a fresh installation uses
the latest compatible versions available from the selected archives.

External programs are feature-dependent:

| Feature | Programs or services |
| --- | --- |
| Search and projects | `git`, `rg`, `fzf`, and standard Unix `find` |
| Environment loading | `direnv` |
| Spelling and dictionaries | the Enchant library for Jinx, `hunspell`, and `sdcv` |
| Prose conversion | `pandoc`; optionally a LanguageTool server at `localhost:8081` |
| LaTeX | a TeX distribution with `xelatex`, `latexmk`, `kpsewhich`, and `texcount`; Evince for PDF viewing |
| Python | `python3`, pylsp, Flake8, Pylint, Ruff, Black, and isort as needed |
| Other languages | R for ESS; Julia; or Scala, Coursier, and Metals as needed |
| Appearance | the `M PLUS Code Latin 50` font, or a replacement configured in `init.el` |
| Remote synchronization | `rsync` and SSH |

The Hunspell and StarDict data files are included in `dict/`; their corresponding
executables or libraries still need to be installed on the host.

## Installation

1. Install Emacs and Git, plus the external tools for the features you intend
   to use.

2. Back up or move both `~/.emacs` and `~/.emacs.d` if they already exist. An
   existing `~/.emacs` can take precedence over this repository's `init.el`.

3. Clone the repository:

   ```sh
   git clone https://github.com/feng-li/.emacs.d.git ~/.emacs.d
   ```

4. Start Emacs once with initialization errors made visible:

   ```sh
   emacs --debug-init
   ```

On the first start for a given Emacs major version, Emacs refreshes the package
archives and installs the packages listed in `package-selected-packages`. This
can take a while. Tree-sitter grammar installation is configured to prompt when
a grammar is first needed.

No submodule initialization is currently required; all active local Lisp used
by the configuration is tracked directly in this repository.

### Optional Python environment

`setup_env.sh` builds the Python environment expected by the Elpy and pylsp
settings:

```sh
bash setup_env.sh
```

The script is deliberately machine-specific. It expects Mamba at
`~/.local/miniforge3/bin/mamba`, creates Python 3.12 under
`~/.local/python3.12`, creates `~/.virtualenvs/lsp`, and installs packages from
the Tsinghua PyPI mirror. Review and edit those paths, the Python version, and
the index URL before running it on another machine.

## State and cache layout

The repository contains source configuration and reusable assets. Generated
state is kept outside it and separated by Emacs major version so that upgrades
do not reuse incompatible packages or native-compiled files.

| Path | Contents |
| --- | --- |
| `~/.config/emacs<major>/elpa/` | installed Emacs packages |
| `~/.config/emacs<major>/eln-cache/` | native-compiled Lisp |
| `~/.config/emacs<major>/tree-sitter/` | tree-sitter grammars |
| `~/.config/emacs<major>/lsp-server/` | language servers managed by LSP mode |
| `~/.config/emacs<major>/<hostname>/` | desktop, history, bookmarks, auto-save metadata, Projectile state, and LSP sessions |

For example, Emacs 31 uses `~/.config/emacs31/`. Host-specific state makes it
possible to use the same configuration on several machines without sharing
open-buffer and history data accidentally.

## Repository layout

| Path | Purpose |
| --- | --- |
| `early-init.el` | package, native-comp, environment, and early startup settings |
| `init.el` | the main configuration and package declarations |
| `site-lisp/` | local Lisp, including `latexmkpvc` and Company RefTeX support |
| `snippets/` | personal Yasnippet templates for LaTeX, Python, and text modes |
| `dict/hunspell/` | bundled US and UK English Hunspell dictionaries |
| `dict/sdcv/` | bundled StarDict dictionaries used by Lexic |
| `dict/mobythesaurus/` | local thesaurus data |
| `setup_env.sh` | optional Python/pylsp environment bootstrap |
| `make_sync.sh` | optional rsync helper for copying the config and versioned state to another host |

Runtime files under paths such as `eln-cache/`, `auto-save-list/`, `request/`,
and `transient/` are ignored by Git.

## Selected key bindings

This list covers the bindings most likely to matter when first using the
configuration; `which-key-mode` is enabled for discovering the rest.

### Global editing and navigation

| Key | Action |
| --- | --- |
| `C-,` | mirror the `C-x` prefix |
| `M-,` | run `counsel-M-x` |
| `C-s` | search with Swiper |
| `C-x C-f` | find a file with Counsel |
| `C-t` | find a file with fzf |
| `C-c g` / `C-c G` | ripgrep search / Git grep |
| `C-c r` | ripgrep in a chosen directory |
| `C-c p` | Projectile command map |
| `C-c t` | toggle the Imenu list |
| `C-c i` | toggle iedit |
| `C->` / `C-<` | mark next / previous matching occurrence |
| `<backtab>` | switch to the previous buffer |
| `<f2>` | move to the next window, including other frames |
| `C-k` | kill the whole line, not just text to end of line |
| `C-z` | undo |
| `M-p` / `C-M-q` | fill / unfill a paragraph |

### Writing and dictionaries

| Key | Action |
| --- | --- |
| `M-4` | correct with Jinx |
| `C-M-$` | select Jinx languages |
| `<f9> 4` | check a word with Ispell/Hunspell |
| `<f9> d` | search the bundled StarDict dictionaries with Lexic |
| `<f9> t` | look up a word with Merriam-Webster Thesaurus |

### LaTeX and BibTeX

| Key | Action |
| --- | --- |
| `<f5>` | run the AUCTeX command; continuous `latexmk -pvc` is the default |
| `C-c C-k` | stop the TeX job and remove intermediate **and output** files |
| `C-c d` | toggle `graphicx` draft mode |
| `C-c w` | count words with `texcount` |
| `C-c c` / `C-c r` / `C-c l` | RefTeX citation / reference / label |
| `C-M-\\` | reformat a BibTeX entry |

The local `latexmkpvc` integration keeps a continuous build alive, retains only
the latest build cycle in the output buffer, reveals failures, jumps to a
parseable source error, and hides automatically opened error output after a
later successful build.

### Language workflows

| Key | Context | Action |
| --- | --- | --- |
| `C-c C-c` | Python/Elpy | send a group and step |
| `C-c C-r` | Python/Elpy | send a region or buffer and step |
| `C-c C-n` | Python | send a line or region and step |
| `C-c C-t` | Python | insert a `pdb` breakpoint |
| `<f9> <f6>` | R/ESS | start R |
| `<f9> r` | R/ESS | switch to the end of the R process buffer |
| `<f4> <f4>` | LSP | describe the item at point |
| `<f9> c` / `<f9> w` | gptel | open chat / rewrite text |

## Personalization

At minimum, review these parts of `init.el` after cloning:

- `user-full-name`, `user-mail-address`, and the default font.
- `package-archives`, especially if the Tsinghua mirrors are not appropriate
  for your location.
- `flycheck-languagetool-url` if no LanguageTool server is running on port
  8081.
- Python executable, virtual-environment, Flake8, and pylsp paths.
- TeX engine and PDF viewer (`xetex` and Evince by default).
- the Coursier and Metals paths used by the Scala LSP setup.
- the gptel backend, proxy host, model names, and API-key source. Do not commit
  personal credentials; keep them in `~/.authinfo`, `~/.authinfo.gpg`, or
  another Emacs auth-source backend.
- the Merriam-Webster integration if you do not want to use the configured API
  access.

`early-init.el` prepends `~/.local/bin`, `~/.cargo/bin`, and
`~/.local/share/coursier/bin` to `PATH`. Adjust this list if GUI Emacs cannot
find the tools installed on your system.

## Updating and troubleshooting

Update the configuration with:

```sh
cd ~/.emacs.d
git pull --ff-only
```

To refresh or repair packages without deleting the versioned package
directory, run `M-x package-refresh-contents` followed by
`M-x package-install-selected-packages`. A new Emacs major version intentionally
uses a new package directory and will bootstrap its packages on first launch.

For startup failures, run `emacs --debug-init` and inspect the first backtrace.
The most common causes on a new machine are an unreachable package mirror, a
missing external executable, or an unedited machine-specific path. Inside
Emacs, `M-: (getenv "PATH")` shows the executable search path Emacs received.

To copy both the repository and the versioned state directory to a remote host:

```sh
bash make_sync.sh user@host
```

The helper follows symlinks and transfers `~/.emacs.d` plus
`~/.config/emacs`; inspect it before use if the remote layout differs.

## Bug reports and third-party material

Report configuration issues at
[github.com/feng-li/.emacs.d/issues](https://github.com/feng-li/.emacs.d/issues).
For third-party packages, bundled code, and dictionary data, use the upstream
project's issue tracker and consult the notices or headers shipped with the
individual files.
