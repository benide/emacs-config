;;; emacsanywhere.el --- Capture and agenda stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame."
  (if (equal "emacs@capture" (frame-parameter nil 'name))
      (delete-other-windows))
  (if (equal "emacs@agenda" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
(after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (when (and (equal "emacs@capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
        (delete-frame)))

(defadvice org-capture-refile
(after delete-capture-frame activate)
  "Advise org-refile to close the frame."
  (if (equal "emacs@capture" (frame-parameter nil 'name))
    (delete-frame)))

(defun activate-capture-frame ()
  "Run `org-capture' in capture frame."
  (select-frame-by-name "emacs@capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture))

;; (defun org-agenda/ben (&optional arg)
;;   "Function to open agenda with specific argument."
;;   (interactive "P")
;;   (org-agenda arg " "))

;; (defadvice org-agenda--quit
;; (after delete-org-agenda-frame activate)
;;   (if (equal "emacs@agenda" (frame-parameter nil 'name))
;;     (delete-frame)))


(provide 'emacsanywhere)
;;; emacsanywhere.el ends here
