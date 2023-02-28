;;; init-other.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(ide/use-package nix-mode :mode "\\.nix\\'")
(ide/use-package scss-mode :mode "\\.scss\\'")
(ide/use-package yaml-mode :mode "\\.yml\\'")
(ide/use-package json-mode :mode "\\.json\\'")
(ide/use-package ess :mode ("\\.[rR]\\'" . R-mode))

(ide/use-package julia-mode :mode "\\.jl\\'")
(ide/use-package julia-repl :hook julia-mode
  :custom
  (julia-repl-executable-records
   '((default "julia")
     (release "julia @release")
     (lts "julia @lts")
     (beta "julia @beta")
     (dev "julia @dev")))
  :config
  (julia-repl-set-terminal-backend 'vterm))

(ide/use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(ide/use-package asy-mode 
  :elpaca (:type git
           :host github
           :repo "vectorgraphics/asymptote"
           :files ("base/*.el"))
  :mode ("\\.asy$" . asy-mode)
  :commands (asy-mode))

;; TODO: org-babel fontification is broken with lilypond
(ide/use-package lilypond-mode
  :elpaca (:type git
;;                  :host nil
;;                  :repo "https://git.savannah.gnu.org/git/lilypond.git"
;;                  :files ("elisp/*.el")
;;                  :pre-build ("python" "scripts/build/lilypond-words.py" "--el" "--dir=elisp/"))
	             :host github
	             :repo "benide/lilypond-mode"
                 :files ("*.el"))
  :mode ("\\.ly$" . LilyPond-mode)
  :commands (LilyPond-mode))

(ide/use-package vterm)

(ide/use-package vterm-toggle
  :after vterm
  :bind (("<f2>" . vterm-toggle-show)
         ;("C-<f2>" . vterm-toggle-cd)
         :map vterm-mode-map
         ("<f2>" . vterm-toggle-hide)
         ("C-<f2>" . vterm-toggle-insert-cd)))


;; TODO: replace ob-julia-vterm with ob-jupyter
;; (ide/use-package julia-vterm)
;; (ide/use-package ob-julia-vterm :elpaca (:type git
;;                                     :host github
;;                                     :repo "shg/ob-julia-vterm.el"
;;                                     ;; :ref  "5893d75cdb9e687b98b99b3675165f4edf0083a6"
;;                                     ))

(ide/use-package magit
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (evil-define-key 'normal with-editor-mode-map
    (kbd "RET") 'with-editor-finish
    [escape] 'with-editor-cancel))

(ide/use-package magit-todos
  :after magit
  :config (magit-todos-mode))

(ide/use-package rg)
(ide/use-package ag)

(ide/use-package diminish)

(ide/use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

(ide/use-package which-key
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

;; (ide/use-package s
;;   :config
;;   (defun ide/s-format-advice (oldfn template a &rest r)
;;     (apply oldfn (replace-regexp-in-string "\\(${my-date-fn}\\)" (ide/date-long) template) a r))
;;   (advice-add 's-format :around #'ide/s-format-advice))

(provide 'init-other)

;;; init-other.el ends here
