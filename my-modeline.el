;; change mode-line color by evil state
(eval-when-compile (require 'cl))
(require 'evil)
(require 'magit)
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-normal-state-p)
                                  '("gray17" . "ivory"))
                                 ((evil-insert-state-p)
                                  '("firebrick" . "ivory"))
                                 ((evil-visual-state-p)
                                  '("DarkSlateGray" . "ivory"))
                                 ((evil-emacs-state-p)
                                  '("SeaGreen" . "ivory"))
                                 ((buffer-modified-p)
                                  '("firebrick" . "ivory"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))
;; (lexical-let ((default-color (cons
;;                       (face-background 'mode-line)
;;                       (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;             (lambda ()
;;               (let ((color (cond ((minibufferp) default-color)
;;                                  ((evil-normal-state-p)
;;                                   '("gray17" . "ivory"))
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
;;                 (set-face-foreground 'mode-line (cdr color))))))

(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun evil-mode-state ()
  (cond ((evil-normal-state-p)   (format "N " ))
        ((evil-visual-state-p)   (format "V " ))
        ((evil-insert-state-p)   (format "I " ))
        ((evil-emacs-state-p)    (format "E " ))
        ((evil-operator-state-p) (format "O " ))
        ((evil-motion-state-p)   (format "M " ))
        ((evil-replace-state-p)  (format "R " ))))

(defun vc-mode-branch-state ()
  (let ((bufname (buffer-file-name (current-buffer))))
    (when bufname
      (when (vc-working-revision bufname)
        (concat (propertize
                 (format "  %s" (magit-get-current-branch))
                 'face 'bold)
                (format " [%s]" (vc-state bufname)))))))

(setq-default mode-line-format
              (list
               mode-line-front-space
               '((:eval (simple-mode-line-render
                         ;; left
                         (format-mode-line
                          (concat
                           (propertize (evil-mode-state)
                                       'face 'bold)
                           "[%b] [%*]"
                           (vc-mode-branch-state)))
                         ;; right
                         (format-mode-line
                          (concat "[%m] "
                                  (format-time-string "%H:%M ")
                                  battery-mode-line-string "]"
                                  "  %l/"
                                  (number-to-string
                                   (count-lines
                                    (point-min)
                                    (point-max))))))))))
