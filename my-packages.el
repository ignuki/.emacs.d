(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") t
(add-to-list 'load-path "~/.emacs.d/lisp/" t)

(defvar prelude-packages
  '(all-the-icons async base16-theme bind-chord bind-key counsel dash diminish
		  epl evil evil-magit fill-column-indicator ghc git-commit
		  goto-chg haskell-mode ivy key-chord less-css-mode magit magit-popup
		  pkg-info projectile swiper undo-tree use-package use-package-chords
		  web-mode with-editor)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (if (member nil '(mapcar 'package-installed-p prelude-packages))
      nil
    t))


  ;; (message "%s" "Emacs Prelude is now refreshing its package database...")
  ;; (package-refresh-contents)
  ;; (message "%s" " done.")
  ;; (dolist (p prelude-packages)
  ;;   (when (not (package-installed-p p))
  ;;     (package-install p)))
