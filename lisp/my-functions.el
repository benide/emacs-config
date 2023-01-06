;;; my-functions.el --- All of my custom functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun ide/date-long (&optional node)
  "Return a detailed date-time string"
  (concat (format-time-string "%Y-%m-%dT%T")
	  ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
	   (format-time-string "%z"))))

(defun ide/insert-date ()
  (interactive)
  (insert (ide/date-long)))

(defun ide/latex-insert-dollar ()
  "Insert \\(\\) and place point in the middle."
  (interactive)
  (insert "\\(\\)")
  (forward-char -2))

(defun ide/latex-insert-doubledollar ()
  "Insert \\[\\] and place point in the middle."
  (interactive)
  (insert "\\[\\]")
  (forward-char -2))

(defun ide/latex-insert-quotes ()
  "Insert ``'' and place point in the middle."
  (interactive)
  (insert "``''")
  (forward-char -2))

;;; stolen from somewhere? can't remember where
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, or
  defun, whichever applies first.  Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(provide 'my-functions)

;;; my-functions.el ends here
