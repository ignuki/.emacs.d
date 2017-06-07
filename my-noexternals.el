(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-to-list 'exec-path "~/.cabal/bin")
(add-to-list
 'default-frame-alist
 '(font . "Iosevka Term 10"))
(set-frame-font
 "Iosevka Term 10")
(setq frame-title-format
      '(buffer-file-name "%f"
			 (dired-directory dired-directory "%b")))
(fringe-mode '(0 . 0))
;; (setq debug-on-error t)
(setq auth-source-save-behavior nil
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-suggest-remove-import-lines t
      haskell-process-type (quote cabal-repl)
      send-mail-function (quote smtpmail-send-it))
(setq enable-local-variables nil)
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks t)
(setq inhibit-compacting-font-caches 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(show-paren-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1
      scroll-conservatively  10000) ;; keyboard scroll one line at a time
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
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

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
	    (c-add-style
	     "libvirt"
	     '((indent-tabs-mode . nil)
	       (c-basic-offset . 4)
	       (c-indent-level . 4)
	       (c-offsets-alist
		(label . 1))))
	    ;; Add kernel style
	    (c-add-style
	     "linux-tabs-only"
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

(add-hook 'c-mode-hook 'my-c-mode-hooks)

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

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l")
       'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t")
       'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i")
       'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c")
       'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c")
       'haskell-process-cabal)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k")
       'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c")
       'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c")
       'haskell-process-cabal)))
