;;; bootstrap-straight.el ---  -*- lexical-binding: t; -*-

;; Bootstrap copied from docs

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Set up straight-use-package

(unless (fboundp 'use-package)
  (straight-use-package 'use-package))
(setq straight-use-package-by-default t)


;; My macro that will not try to reinstall packages that nix has installed

(defmacro ide/use-package (package &rest body)
  (if (locate-library (symbol-name `,package))
      (use-package ,package :straight nil ,@body)
    `(use-package ,package ,@body)))

(provide 'bootstrap-straight)
