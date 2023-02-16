;;; init-completion.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Still need to figure out how consult fits in. Embark and
;; embark-consult for custom buttons, custom actions, for use with
;; bibtex-actions. How do these fit in with org-cite and org-roam? Can
;; we replace helm for bibtex/org-ref? Note the existence of
;; consult-flycheck and consult-lsp as well.

;;; Code:


(savehist-mode 1)

;; some vertico related fixes:
;; https://github.com/raxod502/straight.el/issues/819#issuecomment-882039946
;; https://github.com/raxod502/straight.el#integration-with-use-package-1
;; https://github.com/progfolio/elpaca/issues/10

(ide/use-package vertico
  ;; TODO: figure out how to not use ":elpaca" here
  :elpaca (:files (:defaults "extensions/*"))
  :custom (vertico-cycle t)
  :bind (:map vertico-map
              ;; ("C-e" . vertico-scroll-down)
              ;; ("C-y" . vertico-scroll-up)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init (vertico-mode))

(ide/use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(ide/use-package orderless
  :custom
  (completion-styles '(orderless basic)
   completion-category-overrides '((file (styles basic partial-completion)))))

;; TODO: use consult for more than just search
(ide/use-package consult
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq completion-in-region-function #'consult-completion-in-region))

;; TODO: set up embark
(ide/use-package embark-consult)
(ide/use-package embark)

(provide 'init-completion)

;;; init-completion.el ends here
