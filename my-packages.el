(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") t
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(package-initialize)

(defvar prelude-packages
  '(projectile use-package ivy cl evil magit evil-magit fill-column-indicator
               all-the-icons haskell-mode ghc web-mode less-css-mode base16-emacs)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (if (member
        nil '(mapcar
              'package-installed-p
              prelude-packages))
    nil
    t))

(unless (prelude-packages-installed-p)
 check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
 install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))
