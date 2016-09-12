(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(font . "Iosevka Term-10"))
(set-frame-font "Iosevka Term-10")
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks t)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(show-paren-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1
      scroll-conservatively  10000) ;; keyboard scroll one line at a time
;; theme
(defvar my:theme 'gruvbox)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)
(if (daemonp)
    (add-hook
     'after-make-frame-functions(lambda (frame)
                                  (select-frame frame)
                                  (when (window-system frame)
                                    (set-frame-parameter
                                     (selected-frame) 'internal-border-width 10)
                                    (add-to-list 'default-frame-alist
                                                 '(height . 42))
                                    (add-to-list 'default-frame-alist
                                                 '(width . 84))
                                    (unless my:theme-window-loaded
                                      (if my:theme-terminal-loaded
                                          (enable-theme my:theme)
                                        (load-theme my:theme t))
                                      (setq my:theme-window-loaded t))
                                    (unless my:theme-terminal-loaded
                                      (if my:theme-window-loaded
                                          (enable-theme my:theme)
                                        (load-theme my:theme t))
                                      (setq my:theme-terminal-loaded t)))))
  (progn
    (load-theme my:theme t)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t)))
  (set-frame-parameter (selected-frame) 'internal-border-width 10)
  (add-to-list 'default-frame-alist '(height . 42))
  (add-to-list 'default-frame-alist '(width . 84)))

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq vc-mode 1)
(display-time-mode 1)
(display-battery-mode 1)
