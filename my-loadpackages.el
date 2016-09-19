(load "~/.emacs.d/my-packages.elc")

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
;; (use-package neotree
;;   :init
;;   (setq neo-smart-open 1
;;         neo-auto-indent-point nil
;;         neo-mode-line-type 'none
;;         neo-persist-show nil
;;         neo-window-width 25
;;         neo-banner-message nil
;;         neo-show-updir-line nil)
;;   :config
;;   (add-hook 'neotree-mode-hook (lambda () (setq-local tab-width 1)))
;;   (eval-after-load "neotree" (load "~/.emacs.d/my-neotree.elc"))
;;   )
