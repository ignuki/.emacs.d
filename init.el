(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-check-signature nil)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/" t)
  (add-to-list 'load-path "~/.emacs.d/lisp/" t))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'use-package-chords))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package asm-mode
  :config
  (setq asm-comment-char ?\#)
  :mode ("\\.s\\'" . asm-mode)
  :mode ("\\.S\\'" . asm-mode)
  :mode ("\\.asm\\'" . asm-mode))

(use-package async)

(use-package cargo
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

(use-package dracula-theme
  :config (load-theme 'dracula t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package evil
  :after (:all undo-tree term)
  :init
  (setq evil-want-fine-undo t
	evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'term-mode 'emacs))

(use-package evil-collection
  :after evil
  :init
  (setq evil-magit-state 'normal))

(use-package fill-column-indicator
  :init
  (setq fci-rule-column 90))

(use-package git-commit)

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(use-package goto-chg)

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :interpreter ("haskell" . haskell-mode))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
	ivy-count-format ""
	ivy-display-style nil)
  :config
  (ivy-mode 1))

(use-package less-css-mode
  :mode ("\\.less\\'" . less-css-mode))

(use-package magit)

(use-package magit-popup)

(use-package multi-term
  :after term
  :bind (([f5] . multi-term)
	 ("C-<next>" . multi-term-next)
	 ("C-<prior>" . multi-term-prev))
  :config
  (setq multi-term-buffer-name "term"
	multi-term-program "/bin/bash"))

(use-package nasm-mode
  :mode ("\\.nasm\\'" . nasm-mode))

(use-package pkg-info)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package rust-mode
  :init
  (setq rust-format-on-save t)
  :mode ("\\.rs\\'" . rust-mode))

(use-package s)

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package term
  :config
  (setq term-bind-key-alist
	(list (cons "C-c C-c" 'term-interrupt-subjob)
	      (cons "C-p" 'previous-line)
	      (cons "C-n" 'next-line)
	      (cons "M-f" 'term-send-forward-word)
	      (cons "M-b" 'term-send-backward-word)
	      (cons "C-c C-j" 'term-line-mode)
	      (cons "C-c C-k" 'term-char-mode)
	      (cons "M-DEL" 'term-send-backward-kill-word)
	      (cons "M-d" 'term-send-forward-kill-word)
	      (cons "<C-left>" 'term-send-backward-word)
	      (cons "<C-right>" 'term-send-forward-word)
	      (cons "C-r" 'term-send-reverse-search-history)
	      (cons "M-p" 'term-send-raw-meta)
	      (cons "M-y" 'term-send-raw-meta)
	      (cons "C-y" 'term-send-raw)))
					; https://web.archive.org/web/20181111010613/http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
  (defun term-handle-ansi-terminal-messages (message)
    (while (string-match "\eAnSiT.+\n" message)
      ;; Extract the command code and the argument.
      (let* ((start (match-beginning 0))
	     (command-code (aref message (+ start 6)))
	     (argument
	      (save-match-data
		(substring message
			   (+ start 8)
			   (string-match "\r?\n" message
					 (+ start 8))))))
	;; Delete this command from MESSAGE.
	(setq message (replace-match "" t t message))

	(cond ((= command-code ?c)
	       (setq term-ansi-at-dir argument))
	      ((= command-code ?h)
	       (setq term-ansi-at-host argument))
	      ((= command-code ?u)
	       (setq term-ansi-at-user argument))
	      ((= command-code ?e)
	       (save-excursion
		 (find-file-other-window argument)))
	      ((= command-code ?x)
	       (save-excursion
		 (find-file argument))))))

    (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
      (setq buffer-file-name
	    (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
      (set-buffer-modified-p nil)
      (setq default-directory (if (string= term-ansi-at-host (system-name))
				  (concatenate 'string term-ansi-at-dir "/")
				(format "/%s@%s:%s/" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))))
    message))

(use-package terraform-mode)

(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

(use-package undo-tree
  :config
  (setq undo-limit 40000
	undo-strong-limit 60000)
  undo-tree-auto-save-history       t
  undo-tree-history-directory-alist '(("." . "~/.undo-tree"))
  (global-undo-tree-mode))

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.js\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :mode ("\\.tpl\\.php\\'" . web-mode)
  :mode ("\\.[agj]sp\\'" . web-mode)
  :mode ("\\.as[cp]x\\'" . web-mode)
  :mode ("\\.erb\\'" . web-mode)
  :mode ("\\.mustache\\'" . web-mode)
  :mode ("\\.djhtml\\'" . web-mode)
  :mode ("\\.html?\\'" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2))

(use-package with-editor)

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :mode ("\\.yaml\\'" . yaml-mode))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
      custom-file                       "~/.emacs.d/custom.el"
      auth-source-save-behavior         nil
      enable-local-variables            nil
      inhibit-startup-screen            t
      vc-follow-symlinks                t
      inhibit-compacting-font-caches    1
      mouse-wheel-scroll-amount         '(3 ((shift) . 3))
      mouse-wheel-progressive-speed     nil
      mouse-wheel-follow-mouse          't
      scroll-conservatively             10000
      scroll-step                       1
      auto-save-interval                1000
      auto-window-vscroll               nil
      backup-by-copying                 t
      backup-directory-alist            '(("." . "~/.saves"))
      delete-old-versions               t
      kept-new-versions                 6
      kept-old-versions                 2
      version-control                   t
      backup-directory-alist            `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms    `((".*" ,temporary-file-directory t))
      vc-mode                           1
      ring-bell-function                'ignore
      column-number-mode                1
      savehist-mode                     1)

(setq-default scroll-up-aggressively    0.01
	      scroll-down-aggressively   0.01
	      indent-tabs-mode           t
	      tab-width                  8)

(global-auto-revert-mode 1)
(fringe-mode '(0 . 0))
(show-paren-mode t)
(electric-pair-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(setq blink-cursor-blinks 0)
(display-time-mode 1)

(load "~/.emacs.d/lisp/plsql.el")
(load "~/.emacs.d/lisp/dockerfile-mode.el")
(add-to-list 'auto-mode-alist
	     '("\\.\\(p\\(?:k[bg]\\|ls\\)\\|sql\\)\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

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
     ((string-match "linux/" bname) (c-set-style "linux-tabs-only"))
     ((string-match ".*" bname) (c-set-style "linux"))
     )))

(add-hook 'sh-mode-hook
	  (lambda () (setq indent-tabs-mode nil)))

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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun gcm-scroll-down () (interactive) (scroll-up 1))

(defun gcm-scroll-up () (interactive) (scroll-down 1))

(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)]   'gcm-scroll-up)

(global-set-key [(control next)] 'gcm-scroll-down)
(global-set-key [(control prior)]   'gcm-scroll-up)

(defvar linum-current-line 1 "Current line number.")
(defvar linum-format-fmt   "" " ")
(defvar linum-format "" " ")

(defface linum-current-line
  `((t :inherit linum
       :foreground "chocolate"
       :weight bold
       ))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)))

(add-hook 'linum-before-numbering-hook
	  (lambda ()
	    (setq-local linum-format-fmt
			(let ((w (length (number-to-string (count-lines
							    (point-min)
							    (point-max))))))
			  (concat " %" (number-to-string w) "d ")))))

(defun linum-format-func (line)
  (let ((face
	 (if (= line linum-current-line)
	     'linum-current-line 'linum)))
    (propertize (format linum-format-fmt line) 'face face)))

(setq linum-format 'linum-format-func)

(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))
(add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(linum-mode t)
