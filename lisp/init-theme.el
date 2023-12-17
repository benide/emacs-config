;;; init-theme.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (ide/use-package ef-themes)

(ide/use-package telephone-line
  :demand t
  :config
  (telephone-line-mode 1))

(defun ide/load-theme ()
  "Custom theme loader."
  (interactive)

  (set-face-attribute 'variable-pitch nil :family "Crimson" :height 140)
  ;; (set-face-attribute 'default nil :family "JuliaMono" :height 120)
  ;; (set-face-attribute 'fixed-pitch nil :family "JuliaMono" :height 120)
  (set-face-attribute 'default nil :family "Fira Code" :height 120)
  (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 120)

  ;; TODO: is this still needed to remvoe variable width font from modeline?
  (when (> emacs-major-version 28)
    (set-face-attribute 'mode-line-active nil :inherit 'mode-line))

  (setq modus-themes-org-blocks 'gray-background)
  (load-theme 'modus-vivendi t)

  (when (> emacs-major-version 29)
    ;; copy of modus-themes v3 setup for telephone-line
    ;; only needed for modus-themes v4+ which started shipping with emacs 30
    ;; TODO: these colors don't look as good as what was in v3...
    (modus-themes-with-colors
      (custom-set-faces
        `(telephone-line-accent-active ((,c :background ,fg-mode-line-inactive :foreground ,bg-mode-line-inactive)))
        `(telephone-line-accent-inactive ((,c :background ,bg-mode-line-active :foreground ,fg-mode-line-active)))
        `(telephone-line-error ((,c :inherit bold :foreground ,modeline-err)))
        `(telephone-line-evil ((,c :foreground ,fg-main)))
        `(telephone-line-evil-emacs ((,c :inherit telephone-line-evil :background ,bg-magenta-intense)))
        `(telephone-line-evil-insert ((,c :inherit telephone-line-evil :background ,bg-green-intense)))
        `(telephone-line-evil-motion ((,c :inherit telephone-line-evil :background ,bg-yellow-intense)))

        ;; color should be f0f0f0 with modus-operandi, 191a1b for modus-vivdendi. used to be caled bg-alt
        ;; should I just choose bg-dim instead?
        ;; `(telephone-line-evil-normal ((,c :inherit telephone-line-evil :background "#191a1b")))
        `(telephone-line-evil-normal ((,c :inherit telephone-line-evil :background ,bg-dim)))

        `(telephone-line-evil-operator ((,c :inherit telephone-line-evil :background ,bg-yellow-subtle)))
        `(telephone-line-evil-replace ((,c :inherit telephone-line-evil :background ,bg-red-intense)))
        `(telephone-line-evil-visual ((,c :inherit telephone-line-evil :background ,bg-cyan-intense)))
        `(telephone-line-projectile ((,c :foreground ,modeline-info)))
        `(telephone-line-unimportant ((,c :foreground ,fg-mode-line-inactive)))
        `(telephone-line-warning ((,c :inherit bold :foreground ,modeline-warning)))))))


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
