## Integrating `LatexMk` with `-pvc` option to `AUCTeX`

This is an independent library based on
[nealeyoung's](https://github.com/nealeyoung/auctex-latexmk) pull request which only
requires [tom-tan's] [auctex-latexmk](https://github.com/tom-tan/auctex-latexmk) as an
external dependence.

# Setup

``` lisp
(require 'auctex-latexmk)
(require 'auctex-latexmk-pvc)
(auctex-latexmk-pvc-setup)

;; Use latexmkpvc as the main command
(defun TeX-command-run-latexmkpvc ()
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LatexMkPvc" 'TeX-master-file -1))
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (local-set-key (kbd "<f5>") 'TeX-command-run-latexmkpvc)
             ))

;; Replace LaTeX with latexmk -pvc
(setcdr (assoc "LaTeX" TeX-command-list)
        '("latexmk -pvc -pv- %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t" TeX-run-latexmk-pvc nil
          :help "Run LaTeX with LatexMKPvc"))
```

# Usage

With the above setup, you could just issue `C-c C-a` to start LatexMk with pvc support.
