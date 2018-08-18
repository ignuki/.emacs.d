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
	     (load "~/.emacs.d/lisp/dockerfile-mode.elc")
	     (load "~/.emacs.d/my-gnus.elc")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "f2dd097452b79276ce522df2f8aeb37f6d90f504529616aa46122d549910e46d" "21c6b494da74b10ff573fa96b6850651e321282daa17fc9d6c8fbbee96ade005" default)))
 '(package-selected-packages
   (quote
    (jdee web-mode use-package-chords use-package projectile pkg-info less-css-mode ghc fill-column-indicator evil-magit evil epl diminish dash counsel bind-chord base16-theme async all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
