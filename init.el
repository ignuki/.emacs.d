(package-initialize)

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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
 '(package-selected-packages
   (quote
    (jdee web-mode use-package-chords use-package projectile pkg-info less-css-mode ghc fill-column-indicator evil-magit evil epl diminish dash counsel bind-chord base16-theme async all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
