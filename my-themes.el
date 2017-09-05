(defvar my:theme 'base16-onedark)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)

(defun setcolors ()
  (load-theme my:theme t)
  (custom-theme-set-faces
   my:theme
   '(default ((t (:background "#161718" :foreground "#d0d0d0"))))
   '(hl-line ((t (:background "#282A2C"))))
   '(highlight ((t (:background "#282A2C"))))
   '(linum ((t (:underline nil :background "#232832" :foreground "#a2a3b4"
			   :weight bold))))
   '(mode-line ((t (:background "#181819" :foreground "#b8b8b8" :box nil
				:family "Iosevka Term" :height 100))))
   '(mode-line-buffer-id ((t (:foreground "#a1b56c" :family "Iosevka Term"
					  :height 100))))
   '(mode-line-highlight ((t (:foreground "#ba8baf" :box nil :weight bold
					  :family "Iosevka Term"
					  :height 100))))
   '(mode-line-inactive ((t (:background "#282c34" :foreground "#585858"
					 :box nil :underline nil
					 :family "Iosevka Term" :height 100))))
   )
  )

(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (select-frame frame)
       (when (window-system frame)
         (set-frame-parameter
          (selected-frame) 'internal-border-width 0)
         (unless my:theme-window-loaded
           (if my:theme-terminal-loaded
               (enable-theme my:theme)
             (setcolors))
           (setq my:theme-window-loaded t))
         (unless my:theme-terminal-loaded
           (if my:theme-window-loaded
               (enable-theme my:theme)
             (setcolors))
           (setq my:theme-terminal-loaded t)))))
  (progn
    (setcolors)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t)))
  (set-frame-parameter (selected-frame) 'internal-border-width 0))


(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(height . 42))
              (add-to-list 'default-frame-alist '(width  . 100))))
