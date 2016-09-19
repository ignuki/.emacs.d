(defvar my:theme 'nothing-rhymes-with-orange)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)
(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (select-frame frame)
       (when (window-system frame)
         (set-frame-parameter
          (selected-frame) 'internal-border-width 5)
         (unless my:theme-window-loaded
           (if my:theme-terminal-loaded
               (enable-theme my:theme)
             (load-theme my:theme t))
           (setq my:theme-window-loaded t))
         (unless my:theme-terminal-loaded
           (if my:theme-window-loaded
               (enable-theme my:theme)
             (load-theme my:theme t))
           (setq my:theme-terminal-loaded t)))))
  (progn
    (load-theme my:theme t)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t)))
  (set-frame-parameter (selected-frame) 'internal-border-width 5))

(custom-set-faces
 ;; term
 '(term-color-black
   ((t (:foreground "#525252"))))
 '(term-color-red
   ((t (:foreground "#ff6633"))))
 '(term-color-green
   ((t (:foreground "#bf4c26"))))
 '(term-color-yellow
   ((t (:foreground "#803319"))))
 '(term-color-blue
   ((t (:foreground "#33ddff"))))
 '(term-color-magenta
   ((t (:foreground "#26a6bf"))))
 '(term-color-cyan
   ((t (:foreground "#196f80"))))
 '(term-color-white
   ((t (:foreground "#bfbfbf")))))

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(height . 42))
              (add-to-list 'default-frame-alist '(width  . 84))))