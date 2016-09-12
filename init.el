;;(package-initialize)

(load "~/.emacs.d/my-loadpackages.el")
(add-hook 'after-init-hook '(lambda ()
                              (load "~/.emacs.d/my-noexternals.el")
                              (load "~/.emacs.d/my-setkeys.el")
                              (load "~/.emacs.d/my-modeline.el")
                              (load "~/.emacs.d/my-linum.el")))
