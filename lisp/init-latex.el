;;; init-latex.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(ide/use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; :diminish (visual-line-mode)
  :hook ((LaTeX-mode . reftex-mode)
         (LaTeX-mode .
           (lambda ()
             (TeX-engine-set "luatex")
             (setq TeX-command-default "LatexMk")
             (setq TeX-command-extra-options "-shell-escape")
             (visual-line-mode)
             (LaTeX-math-mode)
             (add-to-list
              'TeX-command-list
              '("Nix build" "nix build"
                TeX-run-command nil (latex-mode)
                :help "Use a nix flake to build"))
             (add-to-list
              'TeX-command-list
              '("LatexMk clean" "latexmk %t -c"
                TeX-run-command nil t
                :help "Delete all auxiliary files"))))
         (LaTeX-mode . auctex-latexmk-setup))
  :custom
  (reftex-plug-into-AUCTeX t)
  (TeX-save-query nil)
  (TeX-auto-save t)
  (TeX-parse-self t)
  ;(TeX-auto-regexp-list 'TeX-auto-full-regexp-list)
  ;; (global-font-lock-mode t)
  (TeX-insert-braces nil)
  (LaTeX-item-indent 2)
  (TeX-electric-escape nil)
  (TeX-electric-math (cons "\\(" ""))
  (TeX-electric-sub-and-superscript nil)
  (TeX-source-correlate-mode t)
  (TeX-interactive-mode t)
  (TeX-PDF-mode t)
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-command-default "LatexMk")
  :init
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(ide/use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil)
  (setq cdlatex-env-alist
	'(("figure" "\\begin{figure}[!htb]
\\centering
\\includegraphics[]{AUTOFILE}
\\caption[]{?}
AUTOLABEL
\\end{figure}" nil)
	   ("figure*" "\\begin{figure*}[!htb]
\\centering
\\includegraphics[]{AUTOFILE}
\\caption[]{?}
AUTOLABEL
\\end{figure}" nil)))

  ;; This is to make `l', `t', `m', `r', and `v' work after typing an apostrophe.
  ;; I can't figure out how to use `cdlatex-math-modify-alist' for the same effect.
  ;; `r' should probably be replaced because \textrm can be useful
  (setf cdlatex-math-modify-alist-default
	(assoc-delete-all ?l cdlatex-math-modify-alist-default))
  (setf cdlatex-math-modify-alist-default
	(assoc-delete-all ?t cdlatex-math-modify-alist-default))
  (setf cdlatex-math-modify-alist-default
	(assoc-delete-all ?m cdlatex-math-modify-alist-default))
  (setf cdlatex-math-modify-alist-default
	(assoc-delete-all ?r cdlatex-math-modify-alist-default))
  (setf cdlatex-math-modify-alist-default
	(assoc-delete-all ?v cdlatex-math-modify-alist-default)))

(ide/use-package auctex-latexmk
  :commands (auctex-latexmk-setup)
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t))



(provide 'init-latex)

;;; init-latex.el ends here
