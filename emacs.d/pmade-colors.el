;; Function to set my color theme
(defun color-theme-pmade ()
  (interactive)
  (custom-set-faces
   '(text-cursor ((t (:background "yellow" :foreground "black"))))
   '(match ((t :background "#4A6152")))
   '(region ((t :background "#505C74")))
   '(hl-line ((t :background "#453232")))
   '(font-lock-comment-face ((t (:italic t :foreground "#B150E7"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "#888888"))))
   '(font-lock-string-face ((t (:foreground "#A5F26E"))))
   '(font-lock-keyword-face ((t (:bold t :foreground "#CC7832"))))
   '(font-lock-warning-face ((t (:underline "red"))))
   '(font-lock-constant-face ((t (:foreground "#6BCFF7"))))
   '(font-lock-type-face ((t (:bold t :foreground "#8888ff"))))
   '(font-lock-variable-name-face ((t (:foreground "#D0D0F3"))))
   '(font-lock-function-name-face ((t (:bold t :foreground "#E8BF6A"))))
   '(font-lock-builtin-face ((t (:foreground "#59ACC2"))))
   '(font-lock-preprocessor-face ((t (:background "#202020"))))
   '(ecb-default-highlight-face ((t (:background "#A5F26E" :foreground "#000000"))))
   '(show-paren-match ((t (:background "#DA44FF" :foreground "#F6CCFF" :bold t))))

   ;; Minibuffer
   '(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))

   ;; Modeline and Things in the Modeline
   '(modeline ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(mode-line-inactive ((t (:background "#4D4D4D" :foreground "#FFFFFF" :box (:line-width 1 :style released-button)))))
   '(modeline-mousable ((t (:background "DarkRed" :foreground "white"))))
   '(modeline-mousable-minor-mode ((t (:background "DarkRed" :foreground "white"))))
   '(window-number-face ((t (:foreground "#FF8E43"))))

   ;; Outline Mode and Org-Mode
   '(outline-1 ((t (:foreground "#D6B163" :bold t))))
   '(outline-2 ((t (:foreground "#A5F26E" :bold t))))
   '(outline-3 ((t (:foreground "#B150E7" :bold nil))))
   '(outline-4 ((t (:foreground "#529DB0" :bold nil))))
   '(outline-5 ((t (:foreground "#CC7832" :bold nil))))
   '(org-level-1 ((t (:inherit outline-1))))
   '(org-level-2 ((t (:inherit outline-2))))
   '(org-level-3 ((t (:inherit outline-3))))
   '(org-level-4 ((t (:inherit outline-4))))
   '(org-level-5 ((t (:inherit outline-5))))
   '(org-agenda-date ((t (:inherit font-lock-type-face))))
   '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
   '(org-scheduled-today ((t (:foreground "#ff6ab9" :italic t))))
   '(org-scheduled-previously ((t (:foreground "#d74b4b"))))
   '(org-upcoming-deadline ((t (:foreground "#d6ff9c"))))
   '(org-warning ((t (:foreground "#8f6aff" :italic t))))
   '(org-date ((t (:inherit font-lock-constant-face))))
   '(org-tag ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-hide ((t (:foreground "#222222"))))
   '(org-todo ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(org-done ((t (:background "DarkGreen" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(org-column ((t (:background "#222222"))))
   '(org-column-title ((t (:background "DarkGreen" :foreground "white" :bold t :box (:line-width 1 :style released-button)))))
   '(org-checkbox ((t (:background "#444444" :foreground "#DDDDDD" :bold t))))
   '(org-mode-line-clock ((t (:height 0.8))))
   '(pmade-org-next-face ((t (:background "#4f97e9" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(pmade-org-pending-face ((t (:background "#d85b00" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(pmade-org-reading-face ((t (:background "#68527a" :foreground "white" :box (:line-width 1 :style released-button)))))

   ;; Muse Mode
   '(muse-header-1 ((t (:inherit outline-1))))
   '(muse-header-2 ((t (:inherit outline-2))))
   '(muse-header-3 ((t (:inherit outline-3))))
   '(muse-header-4 ((t (:inherit outline-4))))
   '(muse-header-5 ((t (:inherit outline-5))))
   '(muse-verbatim ((t (:foreground "#B150E7"))))
   '(muse-link     ((t (:foreground "#5DA8F6"))))
   '(muse-bad-link ((t (:foreground "#FF8882"))))

   ;; ERB (Ruby Embedded in HTML)
   '(erb-face ((t (:background "grey15"))))
   '(erb-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
   '(erb-out-face ((t (:background "grey15"))))
   '(erb-out-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
   '(erb-comment-delim-face ((t (:foreground "#B150E7" :background "grey15"))))
   '(erb-comment-face ((t (:italic t :foreground "#B150E7" :background "grey15"))))

   ;; Diff Mode
   '(diff-added ((t (:foreground "#d7ffaf"))))
   '(diff-changed ((t (:foreground "#ffc28d"))))
   '(diff-removed ((t (:foreground "#ff9999"))))
   '(diff-indicator-added ((t (:background "#d7ffaf" :foreground "#000000"))))
   '(diff-indicator-chnaged ((t (:background "#ffc28d" :foreground "#000000"))))
   '(diff-indicator-removed ((t (:background "#ff9999" :foreground "#000000"))))
   '(diff-context ((t (:foreground "#888888"))))

   ;; Magit (Git GUI)
   '(magit-branch ((t (:foreground "#E07BE0"))))
   '(magit-diff-add ((t (:inherit 'diff-added))))
   '(magit-diff-del ((t (:inherit 'diff-removed))))
   '(magit-diff-file-header ((t (:inherit 'font-lock-constant-face))))
   '(magit-diff-hunk-header ((t (:inherit 'font-lock-keyword-face))))
   '(magit-diff-none ((t (:inherit 'font-lock-comment-delimiter-face))))
   '(magit-header ((t (:inherit 'org-level-1))))
   '(magit-item-highlight ((t (:background "#000000"))))
   
   ;; Compilation
   '(compilation-info ((t (:inherit 'font-lock-string-face :bold t))))
   '(compilation-error ((t (:background "sienna4" :bold t))))
   '(compilation-line-number ((t (:foreground "#FF6666" :bold t))))
   '(flymake-errline ((t :underline "red")))
   '(flymake-warnline ((t :underline "green")))

   ;; nXML
   '(nxml-element-colon-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-element-prefix-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-attribute-value-delimiter-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-cdata-section-content-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-attribute-value-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-attribute-local-name-face ((t (:inherit 'font-lock-constant-face))))
   '(nxml-attribute-local-name-face ((t (:inherit 'font-lock-constant-face))))
   '(nxml-entity-ref-name-face ((t (:inherit 'font-lock-constant-face))))
   '(nxml-element-colon-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-element-prefix-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-element-local-name-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-tag-delimiter-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-tag-slash-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-comment-delimiter-face ((t (:inherit 'font-lock-comment-face))))
   '(nxml-comment-content-face ((t (:inherit 'font-lock-comment-face))))

   ;; ido
   '(ido-first-match ((t (:inherit 'font-lock-string-face))))
   '(ido-subdir ((t (:inherit 'font-lock-function-name-face))))

   ;; ElScreen
   '(elscreen-tab-background-face ((t (:background "#787878" :box (:line-width 1 :style released-button)))))
   '(elscreen-tab-current-screen-face ((t (:background "#969696" :foreground "#000000" :height 1.1 :inherit variable-pitch))))
   '(elscreen-tab-other-screen-face ((t (:background "#787878" :foreground "#444444" :height 1.1 :inherit variable-pitch))))

   ;; ERC
   '(erc-notice-face ((t (:foreground "#444444"))))
   '(erc-my-nick-face ((t (:foreground "#888888" :bold t))))
   '(erc-input-face ((t (:foreground "#A6E2DC"))))
   '(erc-timestamp-face ((t (:foreground "#2d412b"))))
   '(erc-prompt-face ((t (:background "#223c63" :foreground "white" :box (:line-width 1 :style released-button)))))
   
   ;; rcIRC
   '(rcirc-track-nick ((t (:foreground "#FFFFFF" :inverse-video nil :bold t))))
   '(rcirc-track-keyword ((t (:inherit 'rcirc-track-nick))))
   '(rcirc-server ((t (:foreground "#444444"))))
   '(rcirc-timestamp ((t (:foreground "#444444"))))
   '(rcirc-my-nick ((t (:foreground "#888888"))))
   '(rcirc-url ((t (:foreground "#B150E7" :bold t))))
   
   ;; Message mode (mail)
   '(message-header-subject ((t (:foreground "white" :bold t))))
   
   ;; LaTeX
   '(font-latex-sectioning-0-face ((t (:foreground "yellow" :height 1.0))))
   '(font-latex-sectioning-1-face ((t (:foreground "yellow" :height 1.0))))
   '(font-latex-sectioning-2-face ((t (:foreground "yellow" :height 1.0))))
   '(font-latex-sectioning-3-face ((t (:foreground "yellow" :height 1.0))))
   '(font-latex-sectioning-4-face ((t (:foreground "yellow" :height 1.0))))
   '(font-latex-sectioning-5-face ((t (:foreground "yellow" :height 1.0))))
   
   ;; Bookmarks
   '(bm-fringe-face ((t (:foreground "#ff8e43"))))

   ;; Twitter
   '(twit-message-face ((t (:inherit 'default :height 0.8))))
   '(twit-author-face ((t (:inherit 'default :height 0.8))))
   '(twit-title-face ((t (:inherit 'org-column-title))))
   '(twit-logo-face ((t (:inherit 'default))))

   ;; Non-Standard Faces
   '(pmade-fixme-face ((t (:background "#670000" :foreground "#ffffff" :bold t :box (:line-width 1 :color "#ff8e43")))))
   
   ;; Remaining Junk
   '(completion-dynamic-face ((t (:inherit 'match))))
   '(highlight ((t (:inherit 'match))))))

(defun color-theme-pmade-gui ()
  (color-theme-pmade)
  (custom-set-faces
   '(default ((t (:background "#191919" :foreground "#FFFFFF"))))))

(defun color-theme-pmade-terminal ()
  (color-theme-pmade)
  (custom-set-faces
   '(default ((t (:background nil :foreground "brightwhite"))))
   '(font-lock-variable-name-face ((t (:foreground "blue"))))
   '(font-lock-string-face ((t (:foreground "green"))))
   '(font-lock-builtin-face ((t (:foreground "blue" :bold t))))
   '(font-lock-constant-face ((t (:foreground "cyan"))))
   '(font-lock-type-face ((t (:foreground "green" :bold t))))
   '(show-paren-match ((t (:background "red" :foreground "yellow" :bold t))))
   '(font-lock-comment-delimiter-face ((t (:foreground "white"))))
   '(font-lock-comment-face ((t (:italic t :foreground "magenta"))))))

;; Create some faces
(copy-face 'font-lock-warning-face 'pmade-fixme-face)
(make-face 'pmade-org-next-face)
(make-face 'pmade-org-pending-face)
(make-face 'pmade-org-reading-face)
(setq pmade-fixme-face       'pmade-fixme-face
      pmade-org-next-face    'pmade-org-next-face
      pmade-org-pending-face 'pmade-org-pending-face
      pmade-org-reading-face 'pmade-org-reading-face)

 ;; Load color-theme
(if terminal-frame (color-theme-pmade-terminal)
  (color-theme-pmade-gui))

;; Notes
;; * To see all faces in effect: list-faces-display
