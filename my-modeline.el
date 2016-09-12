(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-normal-state-p)
                                  '("gray12" . "ivory"))
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

(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun evil-mode-state ()
  (cond ((evil-normal-state-p)   (format "%s " evil-normal-state-tag))
        ((evil-visual-state-p)   (format "%s " evil-visual-state-tag))
        ((evil-insert-state-p)   (format "%s " evil-insert-state-tag))
        ((evil-emacs-state-p)    (format "%s " evil-emacs-state-tag))
        ((evil-operator-state-p) (format "%s " evil-operator-state-tag))
        ((evil-motion-state-p)   (format "%s " evil-motion-state-tag))
        ((evil-replace-state-p)  (format "%s " evil-replace-state-tag))))

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
