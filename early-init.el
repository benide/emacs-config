;;; early-init.el --- Loads before the gui -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; This file was introduced in Emacs 27. It loads before the gui. Use
;; it to stop package.el from loading (because we use straight.el or
;; elpaca.el instead) and turn off other graphical things so that the
;; gui can load with a little less work.


;;; Code:

;; increase gc threshold to 100 MB during init, 20 MB after. But do
;; note that emacs maintainer John Wigley would suggest resetting to
;; default rather than 20 MB.
(setq gc-cons-threshold (* 100 1024 1024))
(defun my/reset-gc ()
    (setq gc-cons-threshold (* 20 1024 1024)))
(add-hook 'emacs-startup-hook #'my/reset-gc)

;; Don't use package.el
(setq package-enable-at-startup nil)

;; Native comp generates tons of warnings I don't care about.
(setq native-comp-async-report-warnings-errors nil
      native-comp-async-query-on-exit t
      native-comp-deferred-compilation nil)
(setq load-prefer-newer noninteractive)

;; Turn off some things I never use
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(1 . 1)) ;; TODO: is this fringe what I want?
(blink-cursor-mode -1) ;; TODO: do I want a non-blinking cursor?

;; Set up how new frames should look
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; The rest is just copied from doom emacs I think.

(unless (or (daemonp) noninteractive)

  ;; Improves startup speed by not looking a bunch of file handlers
  ;; for every require statement.
  ;; NOTE: Breaks systems that use *.el.gz rather than byte-compiling.
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun doom-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  ;; Report how long it took to load.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done))))

;;; early-init.el ends here
