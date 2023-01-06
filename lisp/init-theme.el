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
