;; Setup package repositories and install use-package if needed

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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
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

;; Configure packages with use-package

(use-package all-the-icons
  :ensure t)

(use-package asm-mode
  :config
  (setq asm-comment-char ?\#)
  :mode (("\\.s\\'" . asm-mode)
	 ("\\.S\\'" . asm-mode)
	 ("\\.asm\\'" . asm-mode)))

(use-package async)

(use-package cc-mode
  :init
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
	   (column (c-langelem-2nd-pos c-syntactic-element))
	   (offset (- (1+ column) anchor))
	   (steps (floor offset c-basic-offset)))
      (* (max steps 1)
	 c-basic-offset)))

  (c-add-style "linux-tabs-only"
	       '("linux" (c-offsets-alist
			  (arglist-cont-nonempty
			   c-lineup-gcc-asm-reg
			   c-lineup-arglist-tabs-only))))

  (defun set-c-styles ()
    (setq indent-tabs-mode t)
    (setq show-trailing-whitespace t)
    (c-set-style "linux-tabs-only"))

  :hook (c-mode . set-c-styles))

(use-package cargo
  :after rust-mode
  :hook rust-mode)

(use-package counsel
  :config
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
        (alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done))
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :hook (find-file . (lambda () (linum-mode 1))))

(use-package dashboard
  :ensure t
  :init
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 1
	dashboard-banner-logo-title "Beep boop."
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-show-shortcuts t
	dashboard-set-navigator t
	dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 10)
			  (bookmarks . 10)
			  (registers . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (add-hook 'server-after-make-frame-hook
	    (lambda ()
	      (switch-to-buffer dashboard-buffer-name)
	      (dashboard-mode)
	      (dashboard-insert-startupify-lists)
	      (dashboard-refresh-buffer))))


(use-package dracula-theme
  :config (load-theme 'dracula t))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package eshell
  :hook (eshell-mode . (lambda () (linum-mode -1))))

(use-package evil
  :after (:all undo-tree)
  :init
  (setq evil-want-fine-undo t
	evil-want-keybinding nil)
  (defun evil-custom-exit ()
    (interactive)
    (if (= (length (window-list)) 1)
	(tab-bar-close-tab)
      (delete-window)))
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map "\C-t" nil)
  (define-key evil-motion-state-map "\C-t" nil)
  (define-key evil-normal-state-map "\C-w" nil)
  (define-key evil-motion-state-map "\C-w" nil)
  (define-key evil-motion-state-map "\C-f" nil)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-ex-define-cmd "q" #'evil-custom-exit))

(use-package evil-collection
  :after (:all evil magit)
  :init
  (setq evil-magit-state 'normal))

(use-package fill-column-indicator
  :config
  (setq fci-rule-column 90))

(use-package git-commit)

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(use-package goto-chg)

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :interpreter ("haskell" . haskell-mode))

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :init
  (defun hs-toggle-hiding-fixed (&optional e)
    "Toggle hiding/showing of a block.
See `hs-hide-block' and `hs-show-block'.
Argument E should be the event that triggered this action."
    (interactive)
    (hs-life-goes-on
     (when e (posn-set-point (event-end e)))
     ;; (posn-set-point (event-end e))
     (if (hs-already-hidden-p)
	 (hs-show-block)
       (hs-hide-block))))

  (defun toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding-fixed)))
  :config
  (advice-add 'hs-toggle-hiding :override #'hs-toggle-hiding-fixed)
  :bind
  (("C-SPC" . toggle-fold)))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
	ivy-count-format ""
	ivy-display-style nil)
  :config
  (ivy-mode 1))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
	 ("\\.json.tmpl\\'" . json-mode)))

(use-package less-css-mode
  :mode ("\\.less\\'" . less-css-mode))

(use-package linum
  :init
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
  :config
  (defadvice linum-update (before advice-linum-update activate)
    "Set the current line."
    (setq linum-current-line (line-number-at-pos)))

  (defun custom-linum-numbering ()
    (setq-local linum-format-fmt
		(let ((width
		       (length (number-to-string (count-lines (point-min) (point-max))))))
		  (concat " %" (number-to-string width) "d "))))

  (defun linum-format-func (line)
    (let ((face
	   (if (= line linum-current-line)
	       'linum-current-line 'linum)))
      (propertize (format linum-format-fmt line) 'face face)))

  (setq linum-format 'linum-format-func)
  (linum-mode t)
  :hook (linum-before-numbering . custom-linum-numbering))

(use-package magit)

(use-package magit-popup)

(use-package mini-frame
  :init
  (setq x-gtk-resize-child-frames 'resize-mode
	mini-frame-show-parameters '((top . 10.0) (width . 1.0) (left . 0.0)))
  :config
  (add-to-list 'mini-frame-ignore-commands 'evil-ex)
  (mini-frame-mode t))

(use-package nasm-mode
  :mode ("\\.nasm\\'" . nasm-mode))

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'server-after-make-frame-hook
	    (lambda ()
	      (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))
  (add-hook 'neotree-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-f") #'neotree-toggle)
	      (local-set-key (kbd "RET") #'neotree-change-root)))
  :bind
  ("C-f" . neotree-find)
  ([f8] . neotree-toggle))

(use-package page-break-lines
  :config
  (page-break-lines-mode t))

(use-package pkg-info)

(use-package plsql
  :load-path "~/.emacs.d/lisp/plsql.el"
  :mode ("\\.plsql\\'" . plsql-mode))

(use-package pragmatapro-lig
  :load-path "~/.emacs.d/lisp/pragmatapro-lig.el"
  :config
  (pragmatapro-lig-global-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  :mode ("\\.rs\\'" . rust-mode))

(use-package s)

(use-package sh-script
  :hook (sh-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package tab-bar
  :after dracula-theme
  :bind
  ("C-t" . tab-bar-new-tab)
  ("C-w" . tab-bar-close-tab)
  ("C-<tab>" . tab-bar-switch-to-next-tab)
  ("C-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-show                     1)
  (tab-bar-close-button-show        nil)
  (tab-bar-new-tab-choice           "*dashboard*")
  (tab-bar-tab-hints                t)
  (tab-bar-separator                "")
  (tab-bar-format                   '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-tab-name-format-function #'custom-tab-bar-tab-name-format-default)
  (tab-bar-close-last-tab-choice    'delete-frame)
  :init
  (defgroup custom-tab-bar nil
    "Custom tweaks to tar-bar-mode."
    :group 'tab-bar)
  (defface custom-tab-bar-tab
    `((t :inherit 'tab-bar-tab
	 :foreground "SeaGreen2"))
    "Face for active tab in tab-bar."
    :group 'custom-tab-bar)
  (defface custom-tab-bar-tab-hint
    `((t :inherit 'custom-tab-bar-tab
	 :foreground "deep pink"))
    "Face for active tab hint in tab-bar."
    :group 'custom-tab-bar)
  (defface custom-tab-bar-tab-inactive
    `((t :inherit 'tab-bar-tab-inactive
	 :foreground "dark gray"))
    "Face for inactive tab in tab-bar."
    :group 'custom-tab-bar)
  (defface custom-tab-bar-tab-hint-inactive
    `((t :inherit 'custom-tab-bar-tab-inactive
	 :foreground "thistle"))
    "Face for inactive tab hint in tab-bar."
    :group 'custom-tab-bar)
  (defun custom-tab-bar-tab-name-format-default (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
	   (tab-face (if (and current-p (display-graphic-p))
			 'custom-tab-bar-tab
		       'custom-tab-bar-tab-inactive))
	   (hint-face (if (and current-p (display-graphic-p))
			  'custom-tab-bar-tab-hint
			'custom-tab-bar-tab-hint-inactive)))
      (concat (propertize (if tab-bar-tab-hints
			      (format "  %d:" (- i 1))
			    "  ")
			  'face hint-face)
	      (propertize
	       (concat
		(alist-get 'name tab)
		(or (and tab-bar-close-button-show
			 (not (eq tab-bar-close-button-show
				  (if current-p 'non-selected 'selected)))
			 tab-bar-close-button)
		    "")
		"  ")
	       'face tab-face))))
  :config
  (tab-bar-mode t))

(use-package terraform-mode)

(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(use-package undo-tree
  :config
  (setq undo-limit 40000
	undo-strong-limit 60000
	undo-tree-auto-save-history t
	undo-tree-history-directory-alist '(("." . "~/.undo-tree")))
  (global-undo-tree-mode))

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2)
  :mode (("\\.html\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)))

(use-package with-editor)

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yml.tmpl\\'" . yaml-mode)
	 ("\\.yaml.tmpl\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode)))

;; Configure emacs general settings with use-package

(use-package emacs
  :init
  (defun remove-elc ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (if (file-exists-p (concat buffer-file-name "c"))
	(delete-file (concat buffer-file-name "c"))))
  (defun gcm-scroll-down () (interactive) (scroll-up 1))
  (defun gcm-scroll-up () (interactive) (scroll-down 1))
  (global-prettify-symbols-mode +1)
  :config
  (add-to-list 'default-frame-alist '(tab-bar . custom-tab-bar))
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
	kmacro-ring-max                   30
	version-control                   t
	backup-directory-alist            `((".*" . ,temporary-file-directory))
	auto-save-file-name-transforms    `((".*" ,temporary-file-directory t))
	vc-mode                           1
	ring-bell-function                'ignore
	column-number-mode                1
	savehist-mode                     1
	x-stretch-cursor                  t
	inhibit-startup-message           t
	initial-scratch-message           ""
	inhibit-startup-echo-area-message t)
  ;;
  (setq-default scroll-up-aggressively   0.01
		scroll-down-aggressively 0.01
		indent-tabs-mode         t
		tab-width                8)
  (set-frame-font "PragmataPro Mono Liga 13" nil t)
  (add-to-list 'default-frame-alist '(font . "PragmataPro Mono Liga 13"))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (set-face-attribute 'tab-bar nil :font "PragmataPro Mono Liga 13")
  (add-hook 'server-after-make-frame-hook
	    (lambda ()
	      (set-face-attribute 'tab-bar nil :font "PragmataPro Mono Liga 13")))
  (global-auto-revert-mode 1)
  (fringe-mode '(0 . 0))
  (global-hl-line-mode 1)
  (blink-cursor-mode 0)
  (setq blink-cursor-blinks 0)
  (display-time-mode 1)
  (electric-pair-mode 1)
  (show-paren-mode t)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  :bind (("C-<down>" . gcm-scroll-down)
	 ("C-<next>" . gcm-scroll-down)
	 ("C-<up>" . gcm-scroll-up)
	 ("C-<prior>" . gcm-scroll-up))
  :hook ((before-save . delete-trailing-whitespace)
	 (after-save . remove-elc)))
