;; Function to set my color theme.  Based on:
;; https://github.com/altercation/solarized.
(defun color-theme-pmade ()
  (interactive)
  (let ((yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
    (custom-set-faces
     '(default        ((t (:background "#002b36" :foreground "#839496"))))
     '(cursor         ((t (:background "#839496" :foreground "#002b36"))))
     `(link           ((t (:underline ,cyan))))
     '(match          ((t (:background "#073642" :foreground "#839496"))))
     '(lazy-highlight ((t (:background "#eee8d5" :foreground "#657b83"))))
     '(isearch        ((t (:background "#d33682" :foreground "#eee8d5"))))
     '(highlight      ((t (:inherit 'match))))
     '(region         ((t (:inherit 'match))))
     '(hl-line        ((t (:inherit 'match))))
     '(fringe         ((t (:inherit 'match))))

     ;; Show paren
     '(show-paren-match ((t (:background "#eee8d5"
                                         :foreground "#268bd2" 
                                         :bold t))))

     ;; Font-lock
     '(font-lock-builtin-face           ((t (:foreground "#2aa198"))))
     '(font-lock-comment-delimiter-face ((t (:foreground "#6c71c4" :bold t))))
     '(font-lock-comment-face           ((t (:foreground "#586e75" :italic t))))
     '(font-lock-constant-face          ((t (:foreground "#b58900"))))
     '(font-lock-function-name-face     ((t (:foreground "#cb4b16" :bold t))))
     '(font-lock-keyword-face           ((t (:foreground "#859900" :bold t))))
     '(font-lock-preprocessor-face      ((t (:foreground "#dc322f"))))
     '(font-lock-string-face            ((t (:foreground "#2aa198"))))
     '(font-lock-type-face              ((t (:foreground "#268bd2" :bold t))))
     '(font-lock-variable-name-face     ((t (:foreground "#d33682"))))
     '(font-lock-warning-face           ((t (:underline  "#dc322f"))))

     ;; Minibuffer
     '(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))

     ;; Modeline and Things in the Modeline
     '(modeline ((t (:background "#b58900" :foreground "#002b36" :box (:line-width 1 :style released-button)))))
     '(mode-line-inactive ((t (:background "#073642" :foreground "#839496" :box (:line-width 1 :style released-button)))))
     '(modeline-mousable ((t (:background "#073642" :foreground "#839496"))))
     '(modeline-mousable-minor-mode ((t (:background "#073642" :foreground "#073642"))))
     
     ;; Flyspell
     '(flyspell-duplicate ((t (:underline "#b58900"))))
     '(flyspell-incorrect ((t (:underline "#dc322f"))))

     ;; Outline Mode
     '(outline-1 ((t (:foreground "#cb4b16" :bold t))))
     '(outline-2 ((t (:foreground "#d33682" :bold t))))
     '(outline-3 ((t (:foreground "#268bd2" :bold nil))))
     '(outline-4 ((t (:foreground "#859900" :bold nil))))
     '(outline-5 ((t (:foreground "#dc322f" :bold nil))))

     ;; Org-Mode
     '(org-level-1               ((t (:inherit 'outline-1))))
     '(org-level-2               ((t (:inherit 'outline-2))))
     '(org-level-3               ((t (:inherit 'outline-3))))
     '(org-level-4               ((t (:inherit 'outline-4))))
     '(org-level-5               ((t (:inherit 'outline-5))))
     '(org-archived              ((t (:inherit 'font-lock-string-face))))
     '(org-document-title        ((t (:inherit 'font-lock-comment-delimiter-face))))
     '(org-document-info-keyword ((t (:inherit 'font-lock-keyword-face))))
     '(org-meta-line             ((t (:inherit 'font-lock-constant-face))))
     '(org-link                  ((t (:inherit 'link))))
     '(org-agenda-date           ((t (:inherit 'outline-1))))
     '(org-agenda-date-weekend   ((t (:inherit 'outline-1))))
     '(org-scheduled-today       ((t (:inherit 'font-lock-comment-face))))
     '(org-scheduled-previously  ((t (:inherit 'font-lock-warning-face))))
     '(org-upcoming-deadline     ((t (:inherit 'font-lock-string-face))))
     '(org-warning               ((t (:inherit 'font-lock-warning-face))))
     '(org-date                  ((t (:inherit 'font-lock-comment-delimiter-face))))
     '(org-tag                   ((t (:inherit font-lock-comment-delimiter-face))))
     '(org-hide                  ((t (:foreground "#073642"))))
     '(org-column                ((t (:background "#073642"))))
     '(org-column-title          ((t (:inherit 'mode-line :background "#cb4b16"))))
     '(org-checkbox              ((t (:inherit 'mode-line :background "#859900"))))
     '(org-todo                  ((t (:inherit 'mode-line :background "#dc322f"))))
     '(org-done                  ((t (:inherit 'mode-line :background "#859900"))))
     '(pmade-org-next-face       ((t (:inherit 'mode-line :background "#2aa198"))))
     '(pmade-org-pending-face    ((t (:inherit 'mode-line :background "#cb4b16"))))
     '(pmade-org-reading-face    ((t (:inherit 'mode-line :background "#6c71c4"))))

     ;; ERB (Ruby Embedded in HTML)
     '(erb-face               ((t (:background "grey15"))))
     '(erb-delim-face         ((t (:foreground "#FFAEEA" :background "grey15"))))
     '(erb-out-face           ((t (:background "grey15"))))
     '(erb-out-delim-face     ((t (:foreground "#FFAEEA" :background "grey15"))))
     '(erb-comment-delim-face ((t (:foreground "#B150E7" :background "grey15"))))
     '(erb-comment-face       ((t (:inherit 'font-lock-comment-face))))

     ;; Diff Mode
     '(diff-added ((t (:foreground "#d7ffaf"))))
     '(diff-changed ((t (:foreground "#ffc28d"))))
     '(diff-removed ((t (:foreground "#ff9999"))))
     '(diff-indicator-added ((t (:background "#d7ffaf" :foreground "#000000"))))
     '(diff-indicator-chnaged ((t (:background "#ffc28d" :foreground "#000000"))))
     '(diff-indicator-removed ((t (:background "#ff9999" :foreground "#000000"))))
     '(diff-context ((t (:foreground "#888888"))))

     ;; Magit (Git GUI)
     '(magit-diff-add         ((t (:inherit 'diff-added))))
     '(magit-diff-del         ((t (:inherit 'diff-removed))))
     '(magit-diff-file-header ((t (:inherit 'font-lock-constant-face))))
     '(magit-diff-hunk-header ((t (:inherit 'font-lock-keyword-face))))
     '(magit-diff-none        ((t (:inherit 'font-lock-comment-delimiter-face))))
     '(magit-branch           ((t (:inherit 'font-lock-comment-face))))
     '(magit-header           ((t (:inherit 'outline-1))))
     '(magit-item-highlight   ((t (:inherit 'font-lock-variable-name-face))))
     
     ;; Compilation
     '(compilation-info ((t (:inherit 'font-lock-string-face :bold t))))
     '(compilation-error ((t (:background "sienna4" :bold t))))
     '(compilation-line-number ((t (:foreground "#FF6666" :bold t))))
     '(flymake-errline ((t :underline "red")))
     '(flymake-warnline ((t :underline "green")))

     ;; nXML
     '(nxml-element-colon-face             ((t (:inherit 'font-lock-type-face))))
     '(nxml-element-prefix-face            ((t (:inherit 'font-lock-keyword-face))))
     '(nxml-attribute-value-delimiter-face ((t (:inherit 'font-lock-string-face))))
     '(nxml-cdata-section-content-face     ((t (:inherit 'font-lock-string-face))))
     '(nxml-attribute-value-face           ((t (:inherit 'font-lock-string-face))))
     '(nxml-attribute-local-name-face      ((t (:inherit 'font-lock-constant-face))))
     '(nxml-entity-ref-name-face           ((t (:inherit 'font-lock-constant-face))))
     '(nxml-element-colon-face             ((t (:inherit 'font-lock-function-name-face))))
     '(nxml-element-prefix-face            ((t (:inherit 'font-lock-function-name-face))))
     '(nxml-element-local-name-face        ((t (:inherit 'font-lock-function-name-face))))
     '(nxml-tag-delimiter-face             ((t (:inherit 'font-lock-function-name-face))))
     '(nxml-tag-slash-face                 ((t (:inherit 'font-lock-function-name-face))))
     '(nxml-comment-delimiter-face         ((t (:inherit 'font-lock-comment-delimiter-face))))
     '(nxml-comment-content-face           ((t (:inherit 'font-lock-comment-face))))

     ;; ido
     '(ido-first-match ((t (:inherit 'font-lock-string-face))))
     '(ido-subdir      ((t (:inherit 'font-lock-function-name-face))))

     ;; rcIRC
     '(rcirc-track-nick    ((t (:foreground "#FFFFFF" :inverse-video nil :bold t))))
     '(rcirc-track-keyword ((t (:inherit 'rcirc-track-nick))))
     '(rcirc-server        ((t (:foreground "#93a1a1"))))
     '(rcirc-timestamp     ((t (:inherit 'rcirc-server))))
     '(rcirc-my-nick       ((t (:foreground "#6c71c4"))))
     '(rcirc-url           ((t (:inherit 'link))))
     
     ;; Message mode (mail)
     '(message-header-subject ((t (:inherit 'default :bold t))))
     
     ;; LaTeX
     '(font-latex-sectioning-0-face ((t (:inherit 'outline-1 :height 1.0))))
     '(font-latex-sectioning-1-face ((t (:inherit 'font-latex-sectioning-0-face))))
     '(font-latex-sectioning-2-face ((t (:inherit 'font-latex-sectioning-0-face))))
     '(font-latex-sectioning-3-face ((t (:inherit 'font-latex-sectioning-0-face))))
     '(font-latex-sectioning-4-face ((t (:inherit 'font-latex-sectioning-0-face))))
     '(font-latex-sectioning-5-face ((t (:inherit 'font-latex-sectioning-0-face))))
     
     ;; Non-Standard Faces
     '(pmade-fixme-face ((t (:background "#dc322f" 
                                         :foreground "#fdf6e3" 
                                         :bold t
                                         :box (:line-width 1 :color "#93a1a1")))))
     
     ;; Remaining Junk
     '(completion-dynamic-face ((t (:inherit 'match)))))))

(defun color-theme-pmade-gui ()
  (color-theme-pmade)
  (custom-set-faces))

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
