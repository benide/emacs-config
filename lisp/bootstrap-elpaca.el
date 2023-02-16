;;; bootstrap-elpaca.el ---  -*- lexical-binding: t; -*-

;; Bootstrap copied from docs

(defvar elpaca-installer-version 0.1)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Set up use-package

(unless (fboundp 'use-package)
  (elpaca use-package (require 'use-package)))
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

;; My macro that will not try to reinstall packages that nix has installed

(defmacro ide/use-package (package &rest body)
  (if (locate-library (symbol-name `,package))
      `(progn
	     (cl-pushnew (quote ,package) elpaca-ignored-dependencies)
	     (use-package ,package :elpaca nil ,@body))
    `(use-package ,package ,@body)))

;; Here is an example that kinda also worked:
;; (if (require 'vterm nil 'noerror)
;;     (cl-pushnew 'vterm elpaca-ignored-dependencies)
;;   (elpaca vterm
;;     (require 'vterm)))

(provide 'bootstrap-elpaca)
