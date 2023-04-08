;;; init-lsp-ts.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; TODO: set up eglot and treesitter

(use-package eglot-jl
  :hook (julia-mode . eglot-jl-init)

  ;; custom timeout is so the language server has time to 
  :custom (eglot-connect-timeout 600))

(provide 'init-lsp-ts)

;;; init-lsp-ts.el ends here
