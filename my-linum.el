(defvar linum-current-line 1 "Current line number.")
(defvar linum-format-fmt   "" " ")
(defvar linum-format "" " ")

(defface linum-current-line
  `((t :inherit linum
       :foreground "chocolate"
       ;; :weight bold
       ))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)))

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines
                                             (point-min)
                                             (point-max))))))
                            (concat "%" (number-to-string w) "d "))))))


(defun linum-format-func (line)
  (let ((face
         (if (= line linum-current-line)
             'linum-current-line
           'linum)))
    (propertize (format linum-format-fmt line) 'face face)))

(unless window-system
  (setq linum-format 'linum-format-func))

(add-hook 'find-file-hook (lambda ()
                            (linum-mode 1)))
(add-hook 'eshell-mode-hook (lambda ()
                              (linum-mode -1)))
