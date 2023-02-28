;;; init-notetools.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(ide/use-package org-roam
  :after org
  :commands
  (org-roam-buffer
   org-roam-buffer-toggle
   org-roam-capture
   org-roam-node-find
   org-roam-node-insert
   org-roam-dailies-capture-today
   org-roam-dailies-goto-yesterday
   org-roam-dailies-goto-date
   org-roam-dailies-goto-today)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-mode-sections
   (list #'org-roam-backlinks-insert-section
         #'org-roam-reflinks-insert-section
         #'org-roam-unlinked-references-insert-section))
  (org-roam-directory (file-truename "~/Projects/notes/roam"))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "${slug}.org"
			 "#+title: ${title}\n#+date: ${ide/date-long}\n#+modified: <>\n")
      :immediate-finish t
      :unnarrowed t)))
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?"
      :if-new (file+head "refs/${slug}.org"
			 "#+title: ${title}\n#+date: ${ide/date-long}\n#+modified: <>\n")
      :unnarrowed t)))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :target (file+head "%<%Y-%m-%d>.org"
    		             "#+title: Daily: %<%Y-%m-%d>\n\n"))))
  :config
  (org-roam-setup)
  ;; (require 'org-roam-protocol)
)

;; TODO: org-roam-protocol?
;; TODO: org-roam-bibtex
;; TODO: org-roam-ui
;; TODO: some combo of bibtex actions, citar, org-cite, consult, embark, etc.

(ide/use-package org-roam-bibtex
  :after org-roam
  )

(ide/use-package citar
  :custom
  (citar-bibliography `(,(expand-file-name "~/Documents/Library/zotero.bib")
                        ,(expand-file-name "~/Documents/Library/calibre.bib")))
  (citar-notes-path (expand-file-name "~/Projects/notes/roam/refs/"))
  (citar-open-note-function 'orb-citar-edit-note)
  (citar-at-point-function 'embark-act)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-global-bibliography `(,(expand-file-name "~/Documents/Library/zotero.bib")
                                  ,(expand-file-name "~/Documents/Library/calibre.bib"))))

(ide/use-package citar-embark
  :after citar embark
  :config (citar-embark-mode))

(ide/use-package citar-org-roam
  :after citar org-roam
  :config (citar-org-roam-mode))


(provide 'init-notetools)

;;; init-notetools.el ends here
