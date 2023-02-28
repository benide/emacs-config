;;; init.el --- Initialization file -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Some basic settings

(fset 'yes-or-no-p 'y-or-n-p)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3))
(pixel-scroll-precision-mode) ;; TODO: does this only work with GTK builds?
(setq scroll-preserve-screen-position 1)
(set-language-environment "UTF-8")
(setq default-input-method "TeX")
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)
(global-auto-revert-mode t)
(delete-selection-mode 1)
(setq user-full-name "Benjamin Ide"
      user-mail-address "ben@bencide.com")
(setq browse-url-browser-function 'browse-url-firefox)
(setq help-window-select t)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; modified from:
;; https://sqrtminusone.xyz/configs/emacs/#custom-frame-title
;; only needed for emacs-i3-integration with older versions of xdotool
(setq-default frame-title-format
	      '("" "%b - emacs@" (:eval (format "%s" (system-name)))))

;; Packages, functions, etc.

(require 'bootstrap-elpaca)
(require 'i3-integration)
(require 'my-functions)
(require 'init-keybindings)
(require 'init-completion)
(require 'init-lsp-ts)
(require 'init-org)
(require 'emacsanywhere)
(require 'init-projects)
(require 'init-other)
(require 'init-latex)
(require 'init-pdftools)
(require 'init-notetools)
(require 'init-elfeed)
(require 'init-theme)
;; (when (file-exists-p custom-file) (load custom-file))


;;; init.el ends here
