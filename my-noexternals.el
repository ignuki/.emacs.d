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

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      undo-tree-history-directory-alist
      '(("." . "~/.undo-tree"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq undo-tree-auto-save-history t)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq vc-mode 1)
(display-time-mode 1)
(display-battery-mode 1)

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local global-hl-line-mode nil)))

(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))
