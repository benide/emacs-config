;;; init-theme.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (elpaca-use-package ef-themes)

(elpaca-use-package telephone-line
  :demand t
  :config
  (telephone-line-mode 1))

(defun ide/load-theme ()
  "Custom theme loader."
  (interactive)
  (set-face-attribute 'variable-pitch nil :family "Crimson" :height 140)
  (set-face-attribute 'default nil :family "JuliaMono" :height 120)
  (set-face-attribute 'fixed-pitch nil :family "JuliaMono" :height 120)
  ;; (set-face-attribute 'default nil :family "Fira Code" :height 120)
  ;; (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 120)
  (when (> emacs-major-version 28)
    (set-face-attribute 'mode-line-active nil :inherit 'mode-line))
  (load-theme 'modus-vivendi t)
  ;; (set-face-attribute 'org-block nil :background "#191a1b")
  ;; (set-face-attribute 'org-block-begin-line nil :background "#191a1b")
  ;; (set-face-attribute 'org-block-end-line nil :background "#191a1b")

  ;; (load-theme 'ef-bio)
)

;;; Colors from modus-themes 3.0 if we update to 4.0 at some point.
;;; Stay on emacs 29 in the future to stay on modus-themes 3 and not worry about this
    ;; `(telephone-line-accent-active ((,class :background ,fg-inactive :foreground ,bg-inactive)))
    ;; `(telephone-line-accent-inactive ((,class :background ,bg-active :foreground ,fg-active)))
    ;; `(telephone-line-error ((,class :inherit bold :foreground ,red-active)))
    ;; `(telephone-line-evil ((,class :foreground ,fg-main)))
    ;; `(telephone-line-evil-emacs ((,class :inherit telephone-line-evil :background ,magenta-intense-bg)))
    ;; `(telephone-line-evil-insert ((,class :inherit telephone-line-evil :background ,green-intense-bg)))
    ;; `(telephone-line-evil-motion ((,class :inherit telephone-line-evil :background ,yellow-intense-bg)))
    ;; `(telephone-line-evil-normal ((,class :inherit telephone-line-evil :background ,bg-alt)))
    ;; `(telephone-line-evil-operator ((,class :inherit telephone-line-evil :background ,yellow-subtle-bg)))
    ;; `(telephone-line-evil-replace ((,class :inherit telephone-line-evil :background ,red-intense-bg)))
    ;; `(telephone-line-evil-visual ((,class :inherit telephone-line-evil :background ,cyan-intense-bg)))
    ;; `(telephone-line-projectile ((,class :foreground ,cyan-active)))
    ;; `(telephone-line-unimportant ((,class :foreground ,fg-inactive)))
    ;; `(telephone-line-warning ((,class :inherit bold :foreground ,yellow-active)))

(defvar ide/is-theme-loaded nil)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (unless ide/is-theme-loaded
                  (if (window-system frame)
                      (progn
                        (ide/load-theme)
                        (setq ide/is-theme-loaded t))))))
  (unless noninteractive (ide/load-theme)))

(provide 'init-theme)

;;; init-theme.el ends here
