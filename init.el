;;(package-initialize)

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(byte-compile-init-dir)
(load "~/.emacs.d/my-loadpackages.elc")
(add-hook 'after-init-hook
          '(lambda ()
             (load "~/.emacs.d/my-noexternals.elc")
             (load "~/.emacs.d/my-setkeys.elc")
             (load "~/.emacs.d/my-modeline.elc")
             (load "~/.emacs.d/my-linum.elc")
             (load "~/.emacs.d/my-themes.elc")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (swiper colemak-evil evil all-the-icons use-package rich-minority projectile mode-icons magit-find-file haskell-mode gruvbox-theme fill-column-indicator evil-magit counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:foreground "#525252"))))
 '(term-color-blue ((t (:foreground "#33ddff"))))
 '(term-color-cyan ((t (:foreground "#196f80"))))
 '(term-color-green ((t (:foreground "#bf4c26"))))
 '(term-color-magenta ((t (:foreground "#26a6bf"))))
 '(term-color-red ((t (:foreground "#ff6633"))))
 '(term-color-white ((t (:foreground "#bfbfbf"))))
 '(term-color-yellow ((t (:foreground "#803319")))))
