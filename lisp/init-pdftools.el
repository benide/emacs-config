;;; init-pdftools.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(elpaca-use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  ;; (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)

  ;; https://github.com/politza/pdf-tools/issues/201#issuecomment-210989952
  ;; removes blinking cursor around pdf pages
  (add-hook 'pdf-view-mode-hook (lambda ()
				  (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))


(provide 'init-pdftools)

;;; init-pdftools.el ends here
