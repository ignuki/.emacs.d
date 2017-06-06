(package-initialize)

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(load "~/.emacs.d/my-packages.el")
(byte-compile-init-dir)
(load "~/.emacs.d/my-loadpackages.elc")
(add-hook 'after-init-hook
          '(lambda ()
             (load "~/.emacs.d/my-noexternals.elc")
             (load "~/.emacs.d/my-setkeys.elc")
             (load "~/.emacs.d/my-modeline.elc")
             (load "~/.emacs.d/my-linum.elc")
             (load "~/.emacs.d/my-themes.elc")
	     (load "~/.emacs.d/lisp/plsql.elc")
	     (load "~/.emacs.d/my-gnus.elc")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   (quote
    ("6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "9955cc54cc64d6c051616dce7050c1ba34efc2b0613d89a70a68328f34e22c8f" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(package-selected-packages
   (quote
    (counsel-projectile swiper colemak-evil evil all-the-icons use-package rich-minority projectile mode-icons magit-find-file haskell-mode gruvbox-theme fill-column-indicator evil-magit counsel)))
 '(safe-local-variable-values (quote ((c-indent-level . 4))))
 '(send-mail-function (quote smtpmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:underline nil))))
 '(mode-line ((t (:background "#282c34" :foreground "#b8b8b8" :box nil :family "Iosevka Term" :height 100))))
 '(mode-line-buffer-id ((t (:foreground "#a1b56c" :family "Iosevka Term" :height 100))))
 '(mode-line-highlight ((t (:foreground "#ba8baf" :box nil :weight bold :family "Iosevka Term" :height 100))))
 '(mode-line-inactive ((t (:background "#282c34" :foreground "#585858" :box nil :underline nil :family "Iosevka Term" :height 100))))
 '(term-color-black ((t (:foreground "#525252"))))
 '(term-color-blue ((t (:foreground "#33ddff"))))
 '(term-color-cyan ((t (:foreground "#196f80"))))
 '(term-color-green ((t (:foreground "#bf4c26"))))
 '(term-color-magenta ((t (:foreground "#26a6bf"))))
 '(term-color-red ((t (:foreground "#ff6633"))))
 '(term-color-white ((t (:foreground "#ede4b1"))))
 '(term-color-yellow ((t (:foreground "#803319")))))
