(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-to-list
 'default-frame-alist
 '(font . "Iosevka Term 10"))
(set-frame-font
 "Iosevka Term 10")

(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
      ;; (setq debug-on-error t)
      auth-source-save-behavior nil
      send-mail-function (quote smtpmail-send-it)
      enable-local-variables nil
      inhibit-startup-screen t
      vc-follow-symlinks t
      inhibit-compacting-font-caches 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1
      scroll-conservatively  10000 ;; keyboard scroll one line at a time
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
      undo-tree-history-directory-alist '(("." . "~/.undo-tree"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      undo-tree-auto-save-history t
      vc-mode 1)

(global-auto-revert-mode 1)
(electric-pair-mode 1)
(show-paren-mode t)
(fringe-mode '(0 . 0))
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(display-time-mode 1)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
	     '("\\.\\(p\\(?:k[bg]\\|ls\\)\\|sql\\)\\'" . plsql-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2))

(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-add-style "libvirt"
			 '((indent-tabs-mode . nil)
			   (c-basic-offset . 4)
			   (c-indent-level . 4)
			   (c-offsets-alist
			    (label . 1))))
	    ;; Add kernel style
	    (c-add-style "linux-tabs-only"
			 '("linux"
			   (c-offsets-alist
			    (arglist-cont-nonempty
			     c-lineup-gcc-asm-reg
			     c-lineup-arglist-tabs-only))))))

(defun my-c-mode-hooks ()
  (let ((bname (buffer-file-name)))
    (cond
     ((string-match "libvirt/" bname) (c-set-style "libvirt"))
     ((string-match "datastructures/" bname) (c-set-style "linux"))
     ((string-match "linux/" bname) (c-set-style "linux-tabs-only"))
     )))

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))
	    nil
	    t))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)
(add-hook 'c-mode-hook 'my-c-mode-hooks)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'eshell-mode-hook (lambda ()
			      (setq-local global-hl-line-mode nil)))

(add-hook 'term-mode-hook (lambda ()
			    (setq-local global-hl-line-mode nil)))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))
