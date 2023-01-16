;;; init-other.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(elpaca-use-package nix-mode :mode "\\.nix\\'")
(elpaca-use-package scss-mode :mode "\\.scss\\'")
(elpaca-use-package yaml-mode :mode "\\.yml\\'")
(elpaca-use-package json-mode :mode "\\.json\\'")
(elpaca-use-package ess :mode ("\\.[rR]\\'" . R-mode))
(elpaca-use-package julia-mode :mode "\\.jl\\'")

(elpaca-use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(elpaca-use-package
  (asy-mode :type git
	        :host github
	        :repo "vectorgraphics/asymptote"
	        :files ("base/*.el"))
  :mode ("\\.asy$" . asy-mode)
  :commands (asy-mode))

;; TODO: org-babel fontification is broken with lilypond
(elpaca-use-package
  (lilypond-mode :type git
;;                  :host nil
;;                  :repo "https://git.savannah.gnu.org/git/lilypond.git"
;;                  :files ("elisp/*.el")
;;                  :pre-build ("python" "scripts/build/lilypond-words.py" "--el" "--dir=elisp/"))
	             :host github
	             :repo "benide/lilypond-mode"
                 :files ("*.el"))
  :mode ("\\.ly$" . LilyPond-mode)
  :commands (LilyPond-mode))

;; note: When I install emacs via nix, I also use nix to install vterm. The following code ensures that when vterm is a dependency of something else, elpaca won't try to re-download and compile it.
;; (if (require 'vterm nil 'noerror)
;;     (cl-pushnew 'vterm elpaca-ignored-dependencies)
;;   (elpaca vterm
;;     (require 'vterm)))
(ide/elpaca-use-package vterm :defer t)

(elpaca-use-package vterm-toggle
  :after vterm
  :bind (("<f2>" . vterm-toggle-show)
         ;("C-<f2>" . vterm-toggle-cd)
         :map vterm-mode-map
         ("<f2>" . vterm-toggle-hide)
         ("C-<f2>" . vterm-toggle-insert-cd)))

;; TODO: replace julia-vterm with julia-repl or julia-snail
(elpaca-use-package julia-vterm)

;; TODO: replace ob-julia-vterm with ob-jupyter
(elpaca-use-package (ob-julia-vterm :type git
                                    :host github
                                    :repo "shg/ob-julia-vterm.el"
                                    ;; :ref  "5893d75cdb9e687b98b99b3675165f4edf0083a6"
                                    ))

(elpaca-use-package magit
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (evil-define-key 'normal with-editor-mode-map
    (kbd "RET") 'with-editor-finish
    [escape] 'with-editor-cancel))

(elpaca-use-package magit-todos
  :after magit
  :config (magit-todos-mode))

(elpaca-use-package rg)
(elpaca-use-package ag)

(elpaca-use-package diminish)

(elpaca-use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

(elpaca-use-package which-key
  :defer 1
  :diminish which-key-mode
  :config (which-key-mode))

;; TODO: do I need any of the following?
;; async
;; f
;; s
;; prce2el
;; ripgrep? even though we have rg?
;; avy
;; nov
;; smartparens
;; shackle
;; erc or rcirc

;; (elpaca-use-package s
;;   :config
;;   (defun ide/s-format-advice (oldfn template a &rest r)
;;     (apply oldfn (replace-regexp-in-string "\\(${my-date-fn}\\)" (ide/date-long) template) a r))
;;   (advice-add 's-format :around #'ide/s-format-advice))

(provide 'init-other)

;;; init-other.el ends here
