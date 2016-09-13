(eval-when-compile
  (require 'cl-lib))

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(defcustom nrwo-contrast 'medium
  "Contrast level for the theme background."
  :options '(soft medium hard))

(deftheme nothing-rhymes-with-orange "Testt")
(let* ((nrwo-dark0_hard  (if (display-graphic-p) "#1d2021" "color-234"))
       (nrwo-dark0       (if (display-graphic-p) "#282828" "color-235"))
       (nrwo-dark0_soft  (if (display-graphic-p) "#32302f" "color-236"))
       (nrwo-dark1       (if (display-graphic-p) "#3c3836" "color-237"))
       (nrwo-dark2       (if (display-graphic-p) "#504945" "color-239"))
       (nrwo-dark3       (if (display-graphic-p) "#665c54" "color-241"))
       (nrwo-dark4       (if (display-graphic-p) "#7c6f64" "color-243"))

       (nrwo-medium      (if (display-graphic-p) "#928374" "color-245")) ;; or 244

       (nrwo-light0_hard (if (display-graphic-p) "#ffffc8" "color-230"))
       (nrwo-light0      (if (display-graphic-p) "#fdf4c1" "color-229"))
       (nrwo-light0_soft (if (display-graphic-p) "#f4e8ba" "color-228"))
       (nrwo-light1      (if (display-graphic-p) "#ebdbb2" "color-223"))
       (nrwo-light2      (if (display-graphic-p) "#d5c4a1" "color-250"))
       (nrwo-light3      (if (display-graphic-p) "#bdae93" "color-248"))
       (nrwo-light4      (if (display-graphic-p) "#a89984" "color-246"))

       (nrwo-bright_red     (if (display-graphic-p) "#fa9632" "color-167"))
       (nrwo-bright_green   (if (display-graphic-p) "#bb7126" "color-142"))
       (nrwo-bright_yellow  (if (display-graphic-p) "#fa952f" "color-214"))
       (nrwo-bright_blue    (if (display-graphic-p) "#a69483" "color-109"))
       (nrwo-bright_purple  (if (display-graphic-p) "#d3869b" "color-175"))
       (nrwo-bright_aqua    (if (display-graphic-p) "#d4ae87" "color-108"))
       (nrwo-bright_orange  (if (display-graphic-p) "#fe8019" "color-208"))

       ;; neutral, no 256-color code, requested, nice work-around meanwhile
       (nrwo-neutral_red    (if (display-graphic-p) "#fa8d34" "#d75f5f"))
       (nrwo-neutral_green  (if (display-graphic-p) "#8a8681" "#afaf00"))
       (nrwo-neutral_yellow (if (display-graphic-p) "#fa8b2f" "#ffaf00"))
       (nrwo-neutral_blue   (if (display-graphic-p) "#fa8b2f" "#87afaf"))
       (nrwo-neutral_purple (if (display-graphic-p) "#d4aa87" "#d787af"))
       (nrwo-neutral_aqua   (if (display-graphic-p) "#bf9a7c" "#87af87"))
       (nrwo-neutral_orange (if (display-graphic-p) "#fe8019" "#ff8700"))

       (nrwo-faded_red      (if (display-graphic-p) "#9e3200" "color-88"))
       (nrwo-faded_green    (if (display-graphic-p) "#782c0e" "color-100"))
       (nrwo-faded_yellow   (if (display-graphic-p) "#b54714" "color-136"))
       (nrwo-faded_blue     (if (display-graphic-p) "#782b07" "color-24"))
       (nrwo-faded_purple   (if (display-graphic-p) "#8f583f" "color-96"))
       (nrwo-faded_aqua     (if (display-graphic-p) "#7a5442" "color-66"))
       (nrwo-faded_orange   (if (display-graphic-p) "#af3a03" "color-130"))

       (nrwo-delimiter-one    (if (display-graphic-p) "#458588" "color-30"))
       (nrwo-delimiter-two    (if (display-graphic-p) "#b16286" "color-168"))
       (nrwo-delimiter-three  (if (display-graphic-p) "#8ec07c" "color-108"))
       (nrwo-delimiter-four   (if (display-graphic-p) "#d65d0e" "color-166"))
       (nrwo-white            (if (display-graphic-p) "#FFFFFF" "white"))
       (nrwo-black            (if (display-graphic-p) "#000000" "black"))
       (nrwo-sienna           (if (display-graphic-p) "#DD6F48" "sienna"))
       (nrwo-darkslategray4   (if (display-graphic-p) "#528B8B" "DarkSlateGray4"))
       (nrwo-lightblue4       (if (display-graphic-p) "#66999D" "LightBlue4"))
       (nrwo-burlywood4       (if (display-graphic-p) "#BBAA97" "burlywood4"))
       (nrwo-aquamarine4      (if (display-graphic-p) "#83A598" "aquamarine4"))
       (nrwo-turquoise4       (if (display-graphic-p) "#61ACBB" "turquoise4"))

       (nrwo-bg (cl-case nrwo-contrast
                  (hard nrwo-dark0_hard)
                  (soft nrwo-dark0_soft)
                  ;; Medium by default.
                  (t    nrwo-dark0))))

  (custom-theme-set-faces
   'nothing-rhymes-with-orange

   ;; UI
   `(default                           ((t (:background ,nrwo-bg :foreground ,nrwo-light0))))
   `(cursor                            ((t (:background ,nrwo-light0))))
   `(mode-line                         ((t (:box nil :background ,nrwo-dark2 :foreground ,nrwo-light2))))
   `(mode-line-inactive                ((t (:box nil :background ,nrwo-dark1 :foreground ,nrwo-light4))))
   `(fringe                            ((t (:background ,nrwo-bg))))
   `(linum                             ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))
   `(hl-line                           ((t (:background ,nrwo-dark1))))
   `(region                            ((t (:background ,nrwo-dark2)))) ;;selection
   `(secondary-selection               ((t (:background ,nrwo-dark1))))
   `(minibuffer-prompt                 ((t (:background ,nrwo-bg :foreground ,nrwo-neutral_green :bold t))))
   `(vertical-border                   ((t (:foreground ,nrwo-dark2))))
   `(link                              ((t (:foreground ,nrwo-faded_blue :underline t))))
   `(shadow                            ((t (:foreground ,nrwo-dark4))))

   ;; Built-in syntax
   `(font-lock-builtin-face            ((t (:foreground ,nrwo-neutral_orange))))
   `(font-lock-constant-face           ((t (:foreground ,nrwo-neutral_purple))))
   `(font-lock-comment-face            ((t (:foreground ,nrwo-dark4))))
   `(font-lock-function-name-face      ((t (:foreground ,nrwo-neutral_yellow))))
   `(font-lock-keyword-face            ((t (:foreground ,nrwo-neutral_red))))
   `(font-lock-string-face             ((t (:foreground ,nrwo-neutral_green))))
   `(font-lock-variable-name-face      ((t (:foreground ,nrwo-neutral_blue))))
   `(font-lock-type-face               ((t (:foreground ,nrwo-neutral_purple))))
   `(font-lock-warning-face            ((t (:foreground ,nrwo-neutral_red :bold t))))

   ;; whitespace-mode
   `(whitespace-space                  ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))
   `(whitespace-hspace                 ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))
   `(whitespace-tab                    ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))
   `(whitespace-newline                ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))
   `(whitespace-trailing               ((t (:background ,nrwo-dark1 :foreground ,nrwo-neutral_red))))
   `(whitespace-line                   ((t (:background ,nrwo-dark1 :foreground ,nrwo-neutral_red))))
   `(whitespace-space-before-tab       ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))
   `(whitespace-indentation            ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))
   `(whitespace-empty                  ((t (:background nil :foreground nil))))
   `(whitespace-space-after-tab        ((t (:background ,nrwo-bg :foreground ,nrwo-dark4))))

   ;; RainbowDelimiters
   `(rainbow-delimiters-depth-1-face   ((t (:foreground ,nrwo-delimiter-one))))
   `(rainbow-delimiters-depth-2-face   ((t (:foreground ,nrwo-delimiter-two))))
   `(rainbow-delimiters-depth-3-face   ((t (:foreground ,nrwo-delimiter-three))))
   `(rainbow-delimiters-depth-4-face   ((t (:foreground ,nrwo-delimiter-four))))
   `(rainbow-delimiters-depth-5-face   ((t (:foreground ,nrwo-delimiter-one))))
   `(rainbow-delimiters-depth-6-face   ((t (:foreground ,nrwo-delimiter-two))))
   `(rainbow-delimiters-depth-7-face   ((t (:foreground ,nrwo-delimiter-three))))
   `(rainbow-delimiters-depth-8-face   ((t (:foreground ,nrwo-delimiter-four))))
   `(rainbow-delimiters-depth-9-face   ((t (:foreground ,nrwo-delimiter-one))))
   `(rainbow-delimiters-depth-10-face  ((t (:foreground ,nrwo-delimiter-two))))
   `(rainbow-delimiters-depth-11-face  ((t (:foreground ,nrwo-delimiter-three))))
   `(rainbow-delimiters-depth-12-face  ((t (:foreground ,nrwo-delimiter-four))))
   `(rainbow-delimiters-unmatched-face ((t (:background nil :foreground ,nrwo-light0))))

   ;; linum-relative
   `(linum-relative-current-face       ((t (:background ,nrwo-dark1 :foreground ,nrwo-light4))))

   ;; Highlight indentation mode
   `(highlight-indentation-current-column-face ((t (:background ,nrwo-dark2 ))))
   `(highlight-indentation-face                ((t (:background ,nrwo-dark1 ))))

   ;; Smartparens
   `(sp-pair-overlay-face              ((t (:background ,nrwo-dark2))))
                                        ;`(sp-wrap-overlay-face             ((t (:inherit sp-wrap-overlay-face))))
                                        ;`(sp-wrap-tag-overlay-face         ((t (:inherit sp-wrap-overlay-face))))
   `(sp-show-pair-match-face           ((t (:background ,nrwo-dark2)))) ;; Pair tags highlight
   `(sp-show-pair-mismatch-face        ((t (:background ,nrwo-neutral_red)))) ;; Highlight for bracket without pair

   ;; elscreen
   `(elscreen-tab-background-face      ((t (:box nil :background ,nrwo-bg)))) ;; Tab bar, not the tabs
   `(elscreen-tab-control-face         ((t (:box nil :background ,nrwo-dark2 :foreground ,nrwo-neutral_red :underline nil)))) ;; The controls
   `(elscreen-tab-current-screen-face  ((t (:box nil :background ,nrwo-dark4 :foreground ,nrwo-dark0)))) ;; Current tab
   `(elscreen-tab-other-screen-face    ((t (:box nil :background ,nrwo-dark2 :foreground ,nrwo-light4 :underline nil)))) ;; Inactive tab

   ;; ag (The Silver Searcher)
   `(ag-hit-face                       ((t (:foreground ,nrwo-neutral_blue))))
   `(ag-match-face                     ((t (:foreground ,nrwo-neutral_red))))

   ;; Diffs
   `(diff-changed                      ((t (:background nil :foreground ,nrwo-light1))))
   `(diff-added                        ((t (:background nil :foreground ,nrwo-neutral_green))))
   `(diff-removed                      ((t (:background nil :foreground ,nrwo-neutral_red))))
   `(diff-indicator-changed            ((t (:inherit diff-changed))))
   `(diff-indicator-added              ((t (:inherit diff-added))))
   `(diff-indicator-removed            ((t (:inherit diff-removed))))

   `(js2-warning                       ((t (:underline (:color ,nrwo-bright_yellow :style wave)))))
   `(js2-error                         ((t (:underline (:color ,nrwo-bright_red :style wave)))))
   `(js2-external-variable             ((t (:underline (:color ,nrwo-bright_aqua :style wave)))))
   `(js2-jsdoc-tag                     ((t (:background nil :foreground ,nrwo-medium ))))
   `(js2-jsdoc-type                    ((t (:background nil :foreground ,nrwo-light4 ))))
   `(js2-jsdoc-value                   ((t (:background nil :foreground ,nrwo-light3 ))))
   `(js2-function-param                ((t (:background nil :foreground ,nrwo-bright_aqua ))))
   `(js2-function-call                 ((t (:background nil :foreground ,nrwo-bright_blue ))))
   `(js2-instance-member               ((t (:background nil :foreground ,nrwo-bright_orange ))))
   `(js2-private-member                ((t (:background nil :foreground ,nrwo-faded_yellow ))))
   `(js2-private-function-call         ((t (:background nil :foreground ,nrwo-faded_aqua ))))
   `(js2-jsdoc-html-tag-name           ((t (:background nil :foreground ,nrwo-light4 ))))
   `(js2-jsdoc-html-tag-delimiter      ((t (:background nil :foreground ,nrwo-light3 ))))


   ;; popup
   `(popup-face                                ((t (:foreground ,nrwo-light1 :background ,nrwo-dark1))))
   `(popup-menu-mouse-face                     ((t (:foreground ,nrwo-light0 :background ,nrwo-faded_green))))
   `(popup-menu-selection-face                 ((t (:foreground ,nrwo-light0 :background ,nrwo-faded_green))))
   `(popup-tip-face                            ((t (:foreground ,nrwo-light2 :background ,nrwo-dark2))))


   ;; helm
   `(helm-M-x-key                              ((t ( :foreground ,nrwo-neutral_orange  ))))
   `(helm-action                               ((t ( :foreground ,nrwo-white :underline t ))))
   `(helm-bookmark-addressbook                 ((t ( :foreground ,nrwo-neutral_red ))))
   `(helm-bookmark-directory                   ((t ( :foreground ,nrwo-bright_purple ))))
   `(helm-bookmark-file                        ((t ( :foreground ,nrwo-faded_blue ))))
   `(helm-bookmark-gnus                        ((t ( :foreground ,nrwo-faded_purple ))))
   `(helm-bookmark-info                        ((t ( :foreground ,nrwo-turquoise4 ))))
   `(helm-bookmark-man                         ((t ( :foreground ,nrwo-sienna ))))
   `(helm-bookmark-w3m                         ((t ( :foreground ,nrwo-neutral_yellow ))))
   `(helm-buffer-directory                     ((t ( :foreground ,nrwo-white         :background ,nrwo-bright_blue  ))))
   `(helm-buffer-not-saved                     ((t ( :foreground ,nrwo-faded_red ))))
   `(helm-buffer-process                       ((t ( :foreground ,nrwo-burlywood4 ))))
   `(helm-buffer-saved-out                     ((t ( :foreground ,nrwo-bright_red ))))
   `(helm-buffer-size                          ((t ( :foreground ,nrwo-bright_purple ))))
   `(helm-candidate-number                     ((t ( :foreground ,nrwo-neutral_green ))))
   `(helm-ff-directory                         ((t ( :foreground ,nrwo-neutral_purple ))))
   `(helm-ff-executable                        ((t ( :foreground ,nrwo-turquoise4  ))))
   `(helm-ff-file                              ((t ( :foreground ,nrwo-sienna ))))
   `(helm-ff-invalid-symlink                   ((t ( :foreground ,nrwo-white         :background ,nrwo-bright_red   ))))
   `(helm-ff-prefix                            ((t ( :foreground ,nrwo-black         :background ,nrwo-neutral_yellow))))
   `(helm-ff-symlink                           ((t ( :foreground ,nrwo-neutral_orange ))))
   `(helm-grep-cmd-line                        ((t ( :foreground ,nrwo-neutral_green ))))
   `(helm-grep-file                            ((t ( :foreground ,nrwo-faded_purple ))))
   `(helm-grep-finish                          ((t ( :foreground ,nrwo-turquoise4 ))))
   `(helm-grep-lineno                          ((t ( :foreground ,nrwo-neutral_orange ))))
   `(helm-grep-match                           ((t ( :foreground ,nrwo-neutral_yellow ))))
   `(helm-grep-running                         ((t ( :foreground ,nrwo-neutral_red ))))
   `(helm-header                               ((t ( :foreground ,nrwo-aquamarine4 ))))
   `(helm-helper                               ((t ( :foreground ,nrwo-aquamarine4 ))))
   `(helm-history-deleted                      ((t ( :foreground ,nrwo-black         :background ,nrwo-bright_red   ))))
   `(helm-history-remote                       ((t ( :foreground ,nrwo-faded_red ))))
   `(helm-lisp-completion-info                 ((t ( :foreground ,nrwo-faded_orange ))))
   `(helm-lisp-show-completion                 ((t ( :foreground ,nrwo-bright_red ))))
   `(helm-locate-finish                        ((t ( :foreground ,nrwo-white         :background ,nrwo-aquamarine4  ))))
   `(helm-match                                ((t ( :foreground ,nrwo-neutral_orange ))))
   `(helm-moccur-buffer                        ((t ( :foreground ,nrwo-bright_aqua :underline t                          ))))
   `(helm-prefarg                              ((t ( :foreground ,nrwo-turquoise4 ))))
   `(helm-selection                            ((t ( :foreground ,nrwo-white         :background ,nrwo-dark2        ))))
   `(helm-selection-line                       ((t ( :foreground ,nrwo-white         :background ,nrwo-dark2        ))))
   `(helm-separator                            ((t ( :foreground ,nrwo-faded_red ))))
   `(helm-source-header                        ((t ( :foreground ,nrwo-light2 ))))
   `(helm-visible-mark                         ((t ( :foreground ,nrwo-black         :background ,nrwo-light3       ))))

   ;; company-mode
   `(company-scrollbar-bg              ((t (:background ,nrwo-dark1))))
   `(company-scrollbar-fg              ((t (:background ,nrwo-dark0_soft))))
   `(company-tooltip                   ((t (:background ,nrwo-dark0_soft))))
   `(company-tooltip-annotation        ((t (:foreground ,nrwo-neutral_green))))
   `(company-tooltip-selection         ((t (:foreground ,nrwo-neutral_purple))))
   `(company-tooltip-common            ((t (:foreground ,nrwo-neutral_blue :underline t))))
   `(company-tooltip-common-selection  ((t (:foreground ,nrwo-neutral_blue :underline t))))
   `(company-preview-common            ((t (:foreground ,nrwo-neutral_purple))))

   ;; Term
   `(term-color-black                  ((t (:foreground ,nrwo-dark1))))
   `(term-color-blue                   ((t (:foreground ,nrwo-neutral_blue))))
   `(term-color-cyan                   ((t (:foreground ,nrwo-neutral_aqua))))
   `(term-color-green                  ((t (:foreground ,nrwo-neutral_green))))
   `(term-color-magenta                ((t (:foreground ,nrwo-neutral_purple))))
   `(term-color-red                    ((t (:foreground ,nrwo-neutral_red))))
   `(term-color-white                  ((t (:foreground ,nrwo-light1))))
   `(term-color-yellow                 ((t (:foreground ,nrwo-neutral_yellow))))
   `(term-default-fg-color             ((t (:foreground ,nrwo-light0))))
   `(term-default-bg-color             ((t (:background ,nrwo-bg))))

   ;; message-mode
   `(message-header-to                 ((t (:inherit font-lock-variable-name-face))))
   `(message-header-cc                 ((t (:inherit font-lock-variable-name-face))))
   `(message-header-subject            ((t (:foreground ,nrwo-neutral_orange :weight bold))))
   `(message-header-newsgroups         ((t (:foreground ,nrwo-neutral_yellow :weight bold))))
   `(message-header-other              ((t (:inherit font-lock-variable-name-face))))
   `(message-header-name               ((t (:inherit font-lock-keyword-face))))
   `(message-header-xheader            ((t (:foreground ,nrwo-faded_blue))))
   `(message-separator                 ((t (:inherit font-lock-comment-face))))
   `(message-cited-text                ((t (:inherit font-lock-comment-face))))
   `(message-mml                       ((t (:foreground ,nrwo-faded_green :weight bold))))

   ;; org-mode
   `(org-hide                          ((t (:foreground ,nrwo-dark0))))
   `(org-level-1                       ((t (:foreground ,nrwo-neutral_blue))))
   `(org-level-2                       ((t (:foreground ,nrwo-neutral_yellow))))
   `(org-level-3                       ((t (:foreground ,nrwo-neutral_purple))))
   `(org-level-4                       ((t (:foreground ,nrwo-neutral_red))))
   `(org-level-5                       ((t (:foreground ,nrwo-neutral_green))))
   `(org-level-6                       ((t (:foreground ,nrwo-neutral_aqua))))
   `(org-level-7                       ((t (:foreground ,nrwo-faded_blue))))
   `(org-level-8                       ((t (:foreground ,nrwo-neutral_orange))))
   `(org-special-keyword               ((t (:inherit font-lock-comment-face))))
   `(org-drawer                        ((t (:inherit font-lock-function-face))))
   `(org-column                        ((t (:background ,nrwo-dark0))))
   `(org-column-title                  ((t (:background ,nrwo-dark0 :underline t :weight bold))))
   `(org-warning                       ((t (:bold t :foreground ,nrwo-neutral_red :weight bold :underline nil))))
   `(org-archived                      ((t (:foreground ,nrwo-light0 :weight bold))))
   `(org-link                          ((t (:foreground ,nrwo-faded_aqua :underline t))))
   `(org-footnote                      ((t (:foreground ,nrwo-neutral_aqua :underline t))))
   `(org-ellipsis                      ((t (:foreground ,nrwo-light4 :underline t))))
   `(org-date                          ((t (:foreground ,nrwo-neutral_blue :underline t))))
   `(org-sexp-date                     ((t (:foreground ,nrwo-faded_blue :underline t))))
   `(org-tag                           ((t (:bold t :weight bold))))
   `(org-list-dt                       ((t (:bold t :weight bold))))
   `(org-todo                          ((t (:bold t :foreground ,nrwo-neutral_red :weight bold))))
   `(org-done                          ((t (:bold t :foreground ,nrwo-neutral_aqua :weight bold))))
   `(org-agenda-done                   ((t (:foreground ,nrwo-neutral_aqua))))
   `(org-headline-done                 ((t (:foreground ,nrwo-neutral_aqua))))
   `(org-table                         ((t (:foreground ,nrwo-neutral_blue))))
   `(org-formula                       ((t (:foreground ,nrwo-neutral_yellow))))
   `(org-document-title                ((t (:foreground ,nrwo-faded_blue))))
   `(org-document-info                 ((t (:foreground ,nrwo-faded_blue))))
   `(org-agenda-structure              ((t (:inherit font-lock-comment-face))))
   `(org-agenda-date-today             ((t (:foreground ,nrwo-light0 :weight bold :italic t))))
   `(org-scheduled                     ((t (:foreground ,nrwo-neutral_yellow))))
   `(org-scheduled-today               ((t (:foreground ,nrwo-neutral_blue))))
   `(org-scheduled-previously          ((t (:foreground ,nrwo-faded_red))))
   `(org-upcoming-deadline             ((t (:inherit font-lock-keyword-face))))
   `(org-deadline-announce             ((t (:foreground ,nrwo-faded_red))))
   `(org-time-grid                     ((t (:foreground ,nrwo-faded_orange))))

   ;; org-habit
   `(org-habit-clear-face              ((t (:background ,nrwo-faded_blue))))
   `(org-habit-clear-future-face       ((t (:background ,nrwo-neutral_blue))))
   `(org-habit-ready-face              ((t (:background ,nrwo-faded_green))))
   `(org-habit-ready-future-face       ((t (:background ,nrwo-neutral_green))))
   `(org-habit-alert-face              ((t (:background ,nrwo-faded_yellow))))
   `(org-habit-alert-future-face       ((t (:background ,nrwo-neutral_yellow))))
   `(org-habit-overdue-face            ((t (:background ,nrwo-faded_red))))
   `(org-habit-overdue-future-face     ((t (:background ,nrwo-neutral_red))))

   ;; elfeed
   `(elfeed-search-title-face          ((t (:foreground ,nrwo-medium))))
   `(elfeed-search-unread-title-face   ((t (:foreground ,nrwo-light0))))
   `(elfeed-search-date-face           ((t (:inherit font-lock-builtin-face :underline t))))
   `(elfeed-search-feed-face           ((t (:inherit font-lock-variable-name-face))))
   `(elfeed-search-tag-face            ((t (:inherit font-lock-keyword-face))))
   `(elfeed-search-last-update-face    ((t (:inherit font-lock-comment-face))))
   `(elfeed-search-unread-count-face   ((t (:inherit font-lock-comment-face))))
   `(elfeed-search-filter-face         ((t (:inherit font-lock-string-face))))

   ;; Smart-mode-line
   `(sml/global            ((t (:foreground ,nrwo-burlywood4 :inverse-video nil))))
   `(sml/modes             ((t (:foreground ,nrwo-bright_green))))
   `(sml/filename          ((t (:foreground ,nrwo-bright_red :weight bold))))
   `(sml/prefix            ((t (:foreground ,nrwo-light1))))
   `(sml/read-only         ((t (:foreground ,nrwo-neutral_blue))))
   `(persp-selected-face   ((t (:foreground ,nrwo-neutral_orange)))))


  )

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'nothing-rhymes-with-orange)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; nrwo-theme.el ends here
