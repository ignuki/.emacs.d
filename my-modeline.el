(eval-when-compile (require 'cl))
(require 'evil)
(require 'magit)
(require 'all-the-icons)

;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                    (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;             (lambda ()
;;               (let ((color (cond ((minibufferp) default-color)
;;                                  ((evil-normal-state-p)
;;                                   '("#181818" . "ivory"))
;;                                  ((evil-insert-state-p)
;;                                   '("firebrick" . "ivory"))
;;                                  ((evil-visual-state-p)
;;                                   '("DarkSlateGray" . "ivory"))
;;                                  ((evil-emacs-state-p)
;;                                   '("SeaGreen" . "ivory"))
;;                                  ((buffer-modified-p)
;;                                   '("firebrick" . "ivory"))
;;                                  (t default-color))))
;;                 (set-face-background 'mode-line (car color))
;;                 (set-face-font 'mode-line "Iosevka Term-10")
;;                 (set-face-font 'mode-line-buffer-id "Iosevka Term-10")
;;                 (set-face-font 'mode-line-emphasis "Iosevka Term-10")
;;                 (set-face-font 'mode-line-highlight "Iosevka Term-10")
;;                 (set-face-font 'mode-line-inactive "Iosevka Term-10")
;;                 (set-face-foreground 'mode-line (cdr color))))))

(defun fancy-mode-line-render (left center right &optional lpad rpad)
  "Return a string the width of the current window with
LEFT, CENTER, and RIGHT spaced out accordingly, LPAD and RPAD,
can be used to add a number of spaces to the front and back of the string."
  (condition-case err
      (let* ((left (if lpad (concat (make-string lpad ?\s) left) left))
             (right (if rpad (concat right (make-string rpad ?\s)) right))
             (width (apply '+ (window-width)
                           (let ((m (window-margins)))
                             (list (or (car m) 0) (or (cdr m) 0)))))
             (total-length (+ (length left) (length center) (length right) 2)))
        (when (> total-length width) (setq left "" right ""))
        (let* ((left-space (/ (- width (length center)) 2))
               (right-space (- width left-space (length center)))
               (lspaces (max (- left-space (length left)) 1))
               (rspaces (max (- right-space (length right)) 1 0)))
          (concat left (make-string lspaces  ?\s)
                  center
                  (make-string rspaces ?\s)
                  right)))
    (error (format "[%s]: (%s) (%s) (%s)" err left center right))))


(defun evil-mode-state ()
  (cond ((evil-normal-state-p)   (format " NORMAL " ))
        ((evil-visual-state-p)   (format " VISUAL " ))
        ((evil-insert-state-p)   (format " INSERT " ))
        ((evil-emacs-state-p)    (format " EMACS " ))
        ((evil-operator-state-p) (format " OPERATOR " ))
        ((evil-motion-state-p)   (format " MOTION " ))
        ((evil-replace-state-p)  (format " REPLACE " ))))

(defun vc-mode-branch-state ()
  (let ((bufname (buffer-file-name (current-buffer))))
    (when bufname
      (when (vc-working-revision bufname)
        (concat
         (propertize (format "  %s" (all-the-icons-octicon "git-branch"))
                     'face `(:height 1.0
                                     :family ,(all-the-icons-octicon-family))
                     'display '(raise 0.0))
         (format " %s [%s]"
                 (propertize (magit-get-current-branch) 'face 'bold)
                 (vc-state bufname)))))))

(defun powerline-modified ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family
             all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family
             all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family
             all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (apply (cadr result) (cddr result))
                'face `(:family ,(funcall (car result))))))

(defun custom-modeline-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize
               icon
               'face
               `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer))
               'display '(raise 0.0))))))

(defun powerline-mode-default ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (when (symbolp icon) ;; This implies it's the major mode
      (propertize
       (format-mode-line "%m ")
       'face `(:height 0.8);; :foreground ,(powerline-fg) :background ,(powerline-c1))
       'display '(raise 0.1)))))

(defun mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display
              `((space :align-to (- (+ right right-fringe right-margin)
                                    ,reserve)))))

(defun line-count-line ()
  (concat (propertize
           (all-the-icons-faicon "rebel")
           'face `(:height 0.8 :background "#86261c") 'display '(raise 0.2))
          (propertize
           (format " %%l/%d  "
                   (count-lines (point-min) (point-max)))
           'face `(:background "#86261c"))))

(defun powerline-time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.0 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string " %H:%M ")
                 'face `(:height 1.0 :background "#86261c"))
     (propertize (format "%s" icon)
                 'face `(:height 1.0 :family "Weather Icons" :background "#86261c")
                 'display '(raise -0.0))
     (propertize " · " 'face `(:height 1.0 :background "#86261c")))))

(setq-default mode-line-format
              (list
               mode-line-front-space
               '((:eval (fancy-mode-line-render
                         ;; left
                         (format-mode-line
                          (concat
                           (powerline-modified)
                           (propertize (evil-mode-state) 'face 'bold)
                           (custom-modeline-mode-icon) ;; %m
                           " %b "
                           (vc-mode-branch-state)))
                         ;;center
                         " "
                         ;; right
                         (format-mode-line
                          (concat
                           ;; (format-time-string "%H:%M ⁃ ")
                           (powerline-time)
                           (line-count-line)))
                         0 0)))))
