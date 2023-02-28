;;; init-keybindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(ide/use-package key-chord
  :demand t
  :custom
  (key-chord-two-keys-delay .05)
  (key-chord-one-key-delay .15)
  :config
  (key-chord-mode 1))

;; (ide/use-package use-package-chords)

(ide/use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  :config (evil-mode 1))

(ide/use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init)
  (evil-set-initial-state 'vterm-mode 'emacs))

(ide/use-package evil-commentary
  :demand t
  :after evil
  :diminish evil-commentary-mode
  :config (evil-commentary-mode))

(ide/use-package evil-surround
  :demand t
  :after evil
  :diminish evil-surround-mode
  :config (global-evil-surround-mode 1))

(ide/use-package general :demand t
  :config

  (general-create-definer ide/leader-def
    :keymaps 'override
    :states '(normal)
    :prefix "SPC")

  ;; Global bindings under leader
  (ide/leader-def
    "x" (general-simulate-key "C-x" :which-key "C-x")
    "c" (general-simulate-key "C-c" :which-key "C-c")
    "h" (general-simulate-key "C-h" :which-key "help")
    "w" (general-simulate-key "C-w" :which-key "windows")
    ";" (general-simulate-key "M-x" :which-key "M-x")
    ;; "/" #'swiper-isearch
    "g" #'magit-status
    "n" #'narrow-or-widen-dwim
    "a" '(:ignore t :which-key "application")
    "af" #'ide/elfeed
    "ae" #'mu4e
    "ai" #'my/erc-start-or-switch
    "ad" #'deft
    "av" #'helm-bibtex
    "ap" #'proced
    "o" '(:ignore t :which-key "org")
    "oc" #'org-capture
    "oa" #'org-agenda
    ;; "b" #'ivy-switch-buffer
    "b" #'consult-buffer
    "p" '(projectile-command-map :which-key "projectile")
    "f" (general-simulate-key "C-x C-f" :which-key "find file")
    ;; "s" #'save-buffer
    "s" '(:ignore t :which-key "search")
    "sg" #'consult-git-grep
    "sr" #'consult-ripgrep
    "ss" #'consult-grep
    "j" '(:ignore t :which-key "jump to...")
    "jj" #'evil-avy-goto-char-timer
    "jw" #'evil-avy-goto-word-1
    "r" '(:ignore t :which-key "roam")
    "rc" #'org-roam-capture
    "rd" #'org-roam-dailies-goto-date
    "rl" #'org-roam-buffer-toggle
    "ro" #'org-roam-node-find
    "rt" #'org-roam-dailies-goto-today
    "rj" #'org-roam-dailies-capture-today
    "rb" #'org-roam-buffer
    "ri" #'org-roam-node-insert
    "rg" #'org-roam-graph
    "l" #'consult-outline)
  ;; available: cdehiklmqtuvyz

  (general-define-key
   "C-x b" #'consult-buffer
   "M-Q" #'unfill-paragraph)

  ;; Org mode
  (general-define-key :keymaps 'org-mode-map
    "$" #'ide/latex-insert-dollar
    "C-\"" #'ide/latex-insert-quotes
    (general-chord "$$") #'ide/latex-insert-doubledollar)

  ;; cdlatex
  (general-define-key :keymaps 'cdlatex-mode-map
    (general-chord "$$") #'ide/latex-insert-doubledollar
    "$" #'ide/latex-insert-dollar)

  ;; Elfeed
  (general-def '(normal visual) elfeed-search-mode-map
    [remap quit-window] #'ide/elfeed-close
    "m" #'elfeed-toggle-star
    "M" #'elfeed-toggle-star)
  (general-def 'normal elfeed-search-mode-map
    "C-<return>" #'ide/elfeed-open-with-eww)

  ;; pdf-tools
  (general-def '(override normal) pdf-view-mode-map
    "d 1" #'bms/pdf-no-filter
    "d 2" #'ide/pdf-basic-dark
    "d 3" #'bms/pdf-midnite-amber
    "d 4" #'bms/pdf-midnite-green
    "d 5" #'bms/pdf-midnite-original
    "d 6" #'bms/pdf-zero-dark
    "." #'pdf-view-auto-slice-minor-mode))


(provide 'init-keybindings)

;;; init-keybindings.el ends here
