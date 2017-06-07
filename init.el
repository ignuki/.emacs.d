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
