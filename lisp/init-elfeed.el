;;; init-elfeed.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(elpaca-use-package elfeed
  :commands (ide/elfeed)
  ;; :hook ((elfeed-show-mode . visual-line-mode)
  ;; 	 (elfeed-show-mode . visual-fill-column-mode))
  ;; :hook ((elfeed-show-mode . olivetti-mode))
  ;; :init
  ;; (defun my/org-version (&optional here full message) "9.4")
  ;; (advice-add 'org-version :override #'my/org-version) 
  :config
  (setq elfeed-db-directory "~/Dropbox/elfeeddb")
  (setq-default elfeed-search-filter "+unread ")
  (setq shr-width 60)

  (defun ide/elfeed ()
    "Load elfeed db, open elfeed, update"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  (defun ide/elfeed-close ()
    "Save elfeed db, close"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defun ide/elfeed-open-with-eww ()
    "Open in eww with `eww-readable'."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
      (eww  (elfeed-entry-link entry))
      (add-hook 'eww-after-render-hook 'eww-readable nil t)))

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star)))

(elpaca-use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/elfeed.org")))


(provide 'init-elfeed)

;;; init-elfeed.el ends here
