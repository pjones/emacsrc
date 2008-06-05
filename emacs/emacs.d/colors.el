;; Function to set my color theme
(defun color-theme-pmade ()
  (interactive)
  (custom-set-faces
   '(text-cursor ((t (:background "yellow" :foreground "black"))))
   '(match ((t :background "#4A6152")))
   '(region ((t :background "#505C74")))
   '(hl-line ((t :background "#000000" :underline "#444444" :bolt t)))
   '(font-lock-comment-face ((t (:italic t :foreground "#B150E7"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "#888888"))))
   '(font-lock-string-face ((t (:foreground "#A5F26E"))))
   '(font-lock-keyword-face ((t (:bold t :foreground "#CC7832"))))
   '(font-lock-warning-face ((t (:underline "red"))))
   '(font-lock-constant-face ((t (:foreground "#6BCFF7"))))
   '(font-lock-type-face ((t (:bold t :foreground "#8888ff"))))
   '(Font-lock-variable-name-face ((t (:foreground "#D0D0F3"))))
   '(font-lock-function-name-face ((t (:bold t :foreground "#E8BF6A"))))
   '(font-lock-builtin-face ((t (:foreground "#59ACC2"))))
   '(font-lock-preprocessor-face ((t (:background "#202020"))))
   '(ecb-default-highlight-face ((t (:background "#A5F26E" :foreground "#000000"))))
   '(show-paren-match ((t (:background "#DA44FF" :foreground "#F6CCFF" :bold t))))
   
   ;; Outline Mode
   '(outline-1 ((t (:foreground "#D6B163" :bold t))))
   '(outline-2 ((t (:foreground "#A5F26E" :bold t))))
   '(outline-3 ((t (:foreground "#B150E7" :bold nil))))
   '(outline-4 ((t (:foreground "#529DB0" :bold nil))))
   '(outline-5 ((t (:foreground "#CC7832" :bold nil))))
   
   ;; Muse Mode
   '(muse-header-1 ((t (:foreground "#D6B163" :height 1.5 :inherit variable-pitch))))
   '(muse-header-2 ((t (:foreground "#A5F26E" :height 1.4 :inherit variable-pitch))))
   '(muse-header-3 ((t (:foreground "#4A77DE" :height 1.3 :inherit variable-pitch))))
   '(muse-header-4 ((t (:foreground "#529DB0" :height 1.2 :inherit variable-pitch))))
   '(muse-header-5 ((t (:foreground "#CC7832" :height 1.1 :inherit variable-pitch))))
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
   
   ;; Compilation
   '(compilation-info ((t (:inherit 'font-lock-string-face :bold t))))
   '(compilation-error ((t (:background "sienna4" :bold t))))
   '(compilation-line-number ((t (:foreground "#FF6666" :bold t))))
   '(flymake-errline ((t :underline "red")))
   '(flymake-warnline ((t :underline "green")))

   ;; MMM
   '(mmm-declaration-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-default-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-output-submode-face  ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-code-submode-face    ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-comment-submode-face ((t (:inherit 'font-lock-comment-face))))

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
   
   ;; JS2
   '(js2-keyword-face ((t :inherit 'font-lock-keyword-face)))
     
   ;; ido
   '(ido-first-match ((t (:inherit 'font-lock-string-face))))
   '(ido-subdir ((t (:inherit 'font-lock-function-name-face))))
   
   ;; ElScreen
   '(elscreen-tab-background-face ((t (:background "#787878" :box (:line-width 1 :style released-button)))))
   '(elscreen-tab-current-screen-face ((t (:background "#969696" :foreground "#000000" :height 1.1 :inherit variable-pitch))))
   '(elscreen-tab-other-screen-face ((t (:background "#787878" :foreground "#444444" :height 1.1 :inherit variable-pitch))))
   
   ;; Minibuffer
   '(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))
   
   ;; Modeline and Things in the Modeline
   '(modeline ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(mode-line-inactive ((t (:background "#4D4D4D" :foreground "#FFFFFF" :box (:line-width 1 :style released-button)))))
   '(modeline-buffer-id ((t (:background "DarkRed" :foreground "white"))))
   '(modeline-mousable ((t (:background "DarkRed" :foreground "white"))))
   '(modeline-mousable-minor-mode ((t (:background "DarkRed" :foreground "white"))))
   '(window-number-face ((t (:foreground "#FF7777"))))

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
  
;; Load color-theme
(if terminal-frame (color-theme-pmade-terminal)
  (color-theme-pmade-gui))

;; Notes
;; * To see all faces in effect: list-faces-display
