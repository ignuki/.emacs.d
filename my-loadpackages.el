(load "~/.emacs.d/my-packages.el")
(use-package ivy
             :init
             (setq ivy-use-virtual-buffers t
                   ivy-count-format ""
                   ivy-display-style nil)
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
             (evil-mode 1))
(use-package magit)
(use-package evil-magit
             :init
             (setq evil-magit-state 'normal))
(use-package neotree
             ;;(setq neo-theme 'file-icons
             :init
             (setq neo-theme 'ascii
                   neo-show-updir-line nil
                   neo-window-width 25
                   neo-smart-open t
                   neo-persist-show nil)
             :config
             (add-hook 'neotree-mode-hook
                       (lambda () (setq-local mode-line-format nil)))
             (add-hook 'neotree-mode-hook
                       (lambda () (setq-local tab-width 2)))
             (advice-add 'display-buffer :around
                         '(lambda (f &rest args)
                            (let ((neotree? (get-buffer-window " *NeoTree*")))
                              (when neotree? (neotree-hide))
                              (apply f args)
                              (select-window
                               (apply 'get-buffer-window args))
                              (when neotree? (neotree-projectile))
                              (select-window
                               (apply 'get-buffer-window args))))))
(use-package fill-column-indicator
             :init
             (setq fci-rule-column 80))
