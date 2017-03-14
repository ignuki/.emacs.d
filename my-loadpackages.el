(load "~/.emacs.d/my-packages.el")

(require 'use-package)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format ""
        ivy-display-style nil)
  :config
  (ivy-mode 1))
(use-package projectile)
(use-package cl)
(use-package evil
  :init
  (setq evil-normal-state-tag    "N"
        evil-insert-state-tag    "I"
        evil-visual-state-tag    "V"
        evil-emacs-state-tag     "E"
        evil-operator-state-tag  "O"
        evil-motion-state-tag    "M"
        evil-replace-state-tag   "R"
        evil-want-fine-undo      'fine )
  :config
  ;;  (add-to-list 'evil-insert-state-modes 'lisp-interaction-mode)
  (evil-mode 1))
(use-package magit)
(use-package evil-magit
  :init
  (setq evil-magit-state 'normal))
(use-package fill-column-indicator
  :init
  (setq fci-rule-column 80))
(use-package all-the-icons)
(use-package haskell-mode)
(use-package ghc)

(use-package web-mode)
(use-package less-css-mode)
