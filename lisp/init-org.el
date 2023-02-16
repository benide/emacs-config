;;; init-org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(ide/use-package org
  :mode (("\\.org\\'" . org-mode))
  :hook ((org-mode . turn-on-org-cdlatex)
         (org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         ;; (org-mode . org-display-inline-images)
         ;; (org-babel-after-execute . org-display-inline-images)
         ;; (org-mode . org-num-mode)
         ;; (org-mode . variable-pitch-mode)
         ;; (org-mode . org-superstar-mode)
         (org-capture-mode . evil-insert-state)
         (org-mode . evil-org-mode))

  :custom
  (org-hide-emphasis-markers nil)
  (org-return-follows-link t)
  (org-highlight-latex-and-related '(latex entities))
  (org-src-fontify-natively t)
  (org-lowest-priority ?D)
  (org-edit-src-content-indentation 0)
  (org-id-link-to-org-use-id t)
  (org-cite-export-processors '((latex . (biblatex "numeric-comp,sorting=none"))
                                (hugo . (csl "ieee.csl")) ;; TODO: does this org-cite exporter work?
                                (t . basic)))
  (org-cite-csl-styles-dir (expand-file-name "~/Documents/Library/Zotero/styles/"))
  (org-cite-global-bibliography `(,(expand-file-name "~/Documents/Library/zotero.bib")
                                  ,(expand-file-name "~/Documents/Library/calibre.bib")))

  :config
  (require 'org-protocol)

  ;; capture templates
  (setq org-capture-templates
	'(("i" "Inbox" entry (file "~/Projects/notes/roam/gtd/inbox.org")
	   "* %<%B %d at %I:%M %p>\n%?")
	  ("t" "To do" entry (file "~/Projects/notes/roam/gtd/todo.org")
	   "* TODO %?")
      ("f" "Food entry" table-line (file+datetree "~/Projects/notes/roam/nutrition.org")
       "| %^{PROMPT|food} | %^{PROMPT|calories} | %^{PROMPT|protein} | %U |" :immediate-finish t)
	  ;; ("p" "Protocol" entry (file+headline "~/Documents/captures.org" "Links")
          ;;  "* [[%:link][%:description]]\n:properties:\n:capture_date: %U\n:end:\n\n#+begin_quote\n%i\n#+end_quote\n%?")
          ;; ("L" "Protocol link" entry (file+headline "~/Documents/captures.org" "Links")
          ;;  "* [[%:link][%:description]]\n:properties:\n:capture_date: %U\n:end:\n%?")
	  ))

  (setq org-agenda-files '("~/Projects/notes/roam/gtd/todo.org"
			   "~/Projects/notes/roam/gtd/projects.org"
			   "~/Projects/notes/roam/gtd/reminders.org"
			   "~/Projects/notes/roam/gtd/schedule.org"))
  (setq org-refile-targets '(("~/Projects/notes/roam/gtd/projects.org" :maxlevel . 3)
			     ("~/Projects/notes/roam/gtd/percolate.org" :level . 1)
			     ("~/Projects/notes/roam/gtd/archive.org")))

  (setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
			    (sequence "PROJECT(p)" "|" "DONE(d)")))

  ;; complete replacement for org-fancy-priorities
  ;; (font-lock-add-keywords 'org-mode
   '(("\\(\\[#A\\]\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) ?⚡) nil)))
     ("\\(\\[#B\\]\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) ?⬆) nil)))
     ("\\(\\[#C\\]\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) ?⬇) nil)))
     ("\\(\\[#D\\]\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) ?☕) nil)))))

(ide/use-package org-contrib
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     ; (julia-vterm . t)
     (asymptote . t)
     (lilypond . t)
     ))
  ; (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  ; (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)
)

;; TODO: why do we need ox-html? I can't remember why this was added
(ide/use-package ox-html :ensure nil :after ox)
(ide/use-package ox-hugo :after ox)
;; TODO: ox-reveal

(ide/use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(ide/use-package time-stamp
  :ensure nil
  :demand t
  :config
  (require 'mode-local)
  (setq time-stamp-active nil)
  (setq-mode-local org-mode
                   time-stamp-active t
                   time-stamp-start "#\\+modified:[ \t]*"
                   time-stamp-end "$"
                   time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]"
                   time-stamp-line-limit 15)
  (add-hook 'before-save-hook #'time-stamp))


(provide 'init-org)

;;; init-org.el ends here
