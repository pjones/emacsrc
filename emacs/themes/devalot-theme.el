;;; devalot-theme.el --- Solarized-based custom theme for faces.
;;
;; Copyright (C) 2007-2012 Peter Jones <pjones@pmade.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary
;;
;; The majority of the colors in this theme come from the Solarized
;; palette, which is released under an MIT license:
;; https://github.com/altercation/solarized.
;;
;;
;;; Code:

(deftheme devalot "A theme loosely based on solarized.")

;; Create the faces if they don't already exist.
(put 'devalot 'theme-immediate t)

;;; Notes
;; * To see all faces in effect: list-faces-display
;; * To see all colors: list-colors-display
(let ((yellow       "#b58900")
      (yellow-high  "#b5a924")
      (orange       "#cb4b16")
      (orange-high  "#cb683f")
      (red          "#dc322f")
      (red-high     "#dc5c5a")
      (magenta      "#d33682")
      (magenta-high "#d36198")
      (violet       "#6c71c4")
      (violet-high  "#9396c4")
      (blue         "#268bd2")
      (blue-high    "#509cd2")
      (cyan         "#2aa198")
      (cyan-high    "#4aa19a")
      (green        "#859900")
      (green-high   "#88b324")
      (bg-normal    "#222222")
      (bg-off       "#003340")
      (bg-high      "#083d4a")
      (bg-low       "#2a2a2a")
      (bg-inverse   "#fdf6e3")
      (fg-normal    "#839496")
      (fg-low       "#586e75")
      (fg-high      "#93a1a1")
      (fg-inverse   "#657b83")
      (class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'devalot
   `(default        ((((type graphic)) (:background ,bg-normal :foreground ,fg-normal))))
   `(cursor         ((t (:background ,magenta-high :foreground ,bg-normal))))
   `(error          ((t (:foreground ,red-high))))
   `(warning        ((t (:foreground ,yellow-high))))
   `(success        ((t (:foreground ,green-high))))
   `(match          ((t (:background ,bg-high :foreground ,fg-normal))))
   `(fringe         ((t (:background ,bg-low :foreground ,fg-low))))
   `(linum          ((t (:background ,bg-low :foreground ,fg-low))))
   `(lazy-highlight ((t (:background ,bg-high :foreground ,magenta-high))))
   `(isearch        ((t (:background ,magenta :foreground ,bg-inverse :bold t))))
   `(region         ((t (:background ,bg-high))))
   `(highlight      ((t (:background ,bg-off))))
   `(hl-line        ((t (:inherit 'highlight))))

   `(link ((,class (:underline  ,blue-high :foreground ,fg-normal))
           (t (:underline "blue" :foreground "blue"))))


   ;; Show paren
   `(show-paren-match ((t (:background ,bg-inverse :foreground ,blue :bold t))))

   ;; Font-lock
   `(font-lock-builtin-face           ((t (:foreground ,fg-low))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-low :bold t))))
   `(font-lock-comment-face           ((t (:foreground ,violet :italic t))))
   `(font-lock-constant-face          ((t (:foreground ,yellow))))
   `(font-lock-function-name-face     ((t (:foreground ,orange :bold t))))
   `(font-lock-keyword-face           ((t (:foreground ,green :bold t))))
   `(font-lock-preprocessor-face      ((t (:foreground ,red))))
   `(font-lock-string-face            ((t (:foreground ,violet-high))))
   `(font-lock-type-face              ((t (:foreground ,blue :bold t))))
   `(font-lock-variable-name-face     ((t (:foreground ,cyan-high))))
   `(font-lock-warning-face           ((t (:foreground ,magenta :background ,bg-off :bold t))))

   ;; Minibuffer
   `(minibuffer-noticeable-prompt ((t (:foreground ,red-high :bold t))))
   `(minibuffer-prompt ((t (:foreground ,magenta :bold t))))

   ;; Modeline and Things in the Modeline
   `(mode-line ((,class (:background ,green :foreground ,bg-normal :box (:line-width 1 :style released-button)))
                (t (:background "green" :foreground "black"))))
   `(mode-line-inactive ((t (:background ,bg-low :foreground ,fg-low :box (:line-width 1 :style released-button)))))
   `(modeline-mousable ((t (:background ,bg-high :foreground ,fg-normal))))
   `(modeline-mousable-minor-mode ((t (:background ,bg-high :foreground ,bg-high))))

   ;; Flyspell
   `(flyspell-duplicate ((t (:foreground ,yellow-high :underline ,yellow))))
   `(flyspell-incorrect ((t (:foreground ,magenta-high :underline ,red))))

   ;; Dired
   `(dired-directory ((t (:foreground ,blue :bold t))))
   `(dired-flagged ((t (:foreground ,magenta-high))))
   `(dired-header ((t (:foreground ,orange))))
   `(dired-ignored ((t (:foreground ,fg-low))))
   `(dired-mark ((t (:foreground ,yellow))))
   `(dired-marked ((t (:foreground ,yellow-high))))
   `(dired-perm-write ((t (:foreground ,fg-high))))
   `(dired-symlink ((t (:foreground ,cyan))))
   `(dired-warning ((t (:underline ,yellow-high))))

   ;; Outline Mode
   `(outline-1 ((,class (:foreground ,blue-high :bold t))
                (t (:foreground "blue" :bold t))))
   `(outline-2 ((,class (:foreground ,green-high :bold t))
                (t (:foreground "green" :bold t))))
   `(outline-3 ((,class (:foreground ,cyan-high :bold nil))
                (t (:foreground "cyan" :bold t))))
   `(outline-4 ((,class (:foreground ,orange :bold nil))
                (t (:foreground "orange" :bold t))))
   `(outline-5 ((,class (:foreground ,yellow-high :bold nil))
                (t (:foreground "magenta"))))

   ;; Org-Mode
   `(org-level-1               ((t (:inherit 'outline-1))))
   `(org-level-2               ((t (:inherit 'outline-2))))
   `(org-level-3               ((t (:inherit 'outline-3))))
   `(org-level-4               ((t (:inherit 'outline-4 :italic t))))
   `(org-level-5               ((t (:inherit 'outline-5))))
   `(org-archived              ((t (:inherit 'font-lock-string-face))))
   `(org-document-title        ((t (:inherit 'font-lock-comment-delimiter-face))))
   `(org-document-info-keyword ((t (:inherit 'font-lock-keyword-face))))
   `(org-meta-line             ((t (:inherit 'font-lock-constant-face))))
   `(org-link                  ((t (:inherit 'link))))
   `(org-agenda-date           ((t (:inherit 'outline-1))))
   `(org-agenda-date-weekend   ((t (:inherit 'outline-1))))
   `(org-agenda-structure      ((t (:foreground ,violet))))
   `(org-agenda-clocking       ((t (:background ,bg-off))))
   `(org-scheduled-today       ((t (:inherit 'font-lock-comment-face))))
   `(org-scheduled-previously  ((t (:inherit 'font-lock-warning-face))))
   `(org-upcoming-deadline     ((t (:inherit 'font-lock-string-face))))
   `(org-warning               ((t (:inherit 'font-lock-warning-face))))
   `(org-date                  ((t (:foreground ,fg-low))))
   `(org-tag                   ((t (:foreground ,fg-low))))
   `(org-tag-default           ((t (:inherit 'org-tag))))
   `(org-column                ((t (:background ,bg-low))))
   `(org-column-title          ((t (:inherit 'mode-line :background ,orange-high))))
   `(org-checkbox              ((t (:inherit 'mode-line :background ,green))))
   `(org-todo                  ((t (:inherit 'mode-line :background ,red))))
   `(org-done                  ((t (:inherit 'mode-line :background ,green))))

   `(org-hide ((,class (:foreground ,bg-off))
               (t (:foreground "black"))))

   `(org-special-keyword ((,class (:foreground ,fg-low))
                          (t (:foreground "gray"))))

   `(org-code ((,class (:inherit 'font-lock-keyword-face :bold nil))
               (nil (:foreground "green"))))

   `(org-block ((t (:inherit 'org-code))))
   `(org-block-begin-line ((t (:inherit 'org-special-keyword))))
   `(org-block-end-line ((t (:inherit 'org-block-begin-line))))


   ;; ERB (Ruby Embedded in HTML)
   `(erb-face               ((t (:background ,bg-normal :foreground ,fg-normal))))
   `(erb-delim-face         ((,class (:foreground ,fg-low)) (t (:foreground "magenta"))))
   `(erb-out-face           ((t (:background ,bg-normal :foreground ,fg-normal))))
   `(erb-out-delim-face     ((,class (:foreground ,blue :background ,bg-normal)) (t (:foreground "blue"))))
   `(erb-comment-delim-face ((t (:foreground ,fg-low :bold t))))
   `(erb-comment-face       ((t (:foreground ,violet :italic t))))

   ;; Diff Mode
   `(diff-added             ((t (:foreground ,green))))
   `(diff-changed           ((t (:foreground ,yellow))))
   `(diff-removed           ((t (:foreground ,red))))
   `(diff-indicator-added   ((t (:foreground ,green  :background ,bg-low))))
   `(diff-indicator-chnaged ((t (:foreground ,yellow :background ,bg-low))))
   `(diff-indicator-removed ((t (:foreground ,red    :background ,bg-low))))
   `(diff-context           ((t (:foreground ,fg-low))))

   ;; Magit (Git GUI)
   `(magit-diff-add         ((t (:inherit 'diff-added))))
   `(magit-diff-del         ((t (:inherit 'diff-removed))))
   `(magit-diff-file-header ((t (:inherit 'font-lock-constant-face))))
   `(magit-diff-hunk-header ((t (:inherit 'font-lock-keyword-face))))
   `(magit-diff-none        ((t (:inherit 'font-lock-comment-delimiter-face))))
   `(magit-branch           ((t (:inherit 'font-lock-comment-face))))
   `(magit-header           ((t (:inherit 'outline-1))))
   `(magit-item-highlight   ((t (:background ,bg-low))))

   ;; Compilation
   `(compilation-info ((t (:inherit 'font-lock-string-face :bold t))))
   `(compilation-error ((t (:underline ,red :bold t))))
   `(compilation-line-number ((t (:foreground ,orange :bold t))))
   `(flymake-errline ((t :underline ,magenta-high :background ,bg-off, :foreground ,magenta-high)))
   `(flymake-warnline ((t :underline ,yellow ,bg-normal)))

   ;; nXML
   `(nxml-element-colon-face             ((t (:inherit 'font-lock-type-face))))
   `(nxml-element-prefix-face            ((t (:inherit 'font-lock-keyword-face))))
   `(nxml-attribute-value-delimiter-face ((t (:inherit 'font-lock-string-face))))
   `(nxml-cdata-section-content-face     ((t (:inherit 'font-lock-string-face))))
   `(nxml-attribute-value-face           ((t (:inherit 'font-lock-string-face))))
   `(nxml-attribute-local-name-face      ((t (:inherit 'font-lock-constant-face))))
   `(nxml-entity-ref-name-face           ((t (:inherit 'font-lock-constant-face))))
   `(nxml-element-colon-face             ((t (:inherit 'font-lock-function-name-face))))
   `(nxml-element-prefix-face            ((t (:inherit 'font-lock-function-name-face))))
   `(nxml-element-local-name-face        ((t (:inherit 'font-lock-function-name-face))))
   `(nxml-tag-delimiter-face             ((t (:inherit 'font-lock-function-name-face))))
   `(nxml-tag-slash-face                 ((t (:inherit 'font-lock-function-name-face))))
   `(nxml-comment-delimiter-face         ((t (:inherit 'font-lock-comment-delimiter-face))))
   `(nxml-comment-content-face           ((t (:inherit 'font-lock-comment-face))))

   ;; ido
   `(ido-first-match ((t (:foreground ,magenta-high))))
   `(ido-only-match  ((t (:foreground ,green :bold t))))
   `(ido-subdir      ((t (:foreground ,blue :bold t))))
   `(ido-virtual     ((t (:foreground ,fg-low))))

   ;; rcIRC
   `(rcirc-track-nick    ((t (:foreground ,fg-low :bold t))))
   `(rcirc-track-keyword ((t (:inherit 'rcirc-track-nick))))
   `(rcirc-server        ((t (:foreground ,fg-high))))
   `(rcirc-timestamp     ((t (:inherit 'rcirc-server))))
   `(rcirc-my-nick       ((t (:foreground ,violet))))
   `(rcirc-url           ((t (:inherit 'link))))

   ;; Message mode (mail)
   `(message-header-subject ((t (:inherit 'default :bold t))))

   ;; LaTeX
   `(font-latex-sectioning-0-face ((t (:inherit 'outline-1 :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:inherit 'font-latex-sectioning-0-face))))
   `(font-latex-sectioning-2-face ((t (:inherit 'font-latex-sectioning-0-face))))
   `(font-latex-sectioning-3-face ((t (:inherit 'font-latex-sectioning-0-face))))
   `(font-latex-sectioning-4-face ((t (:inherit 'font-latex-sectioning-0-face))))
   `(font-latex-sectioning-5-face ((t (:inherit 'font-latex-sectioning-0-face))))

   ;; Whitespace Mode
   `(whitespace-tab ((t (:background ,bg-low))))
   `(whitespace-trailing ((t (:background ,bg-normal :foreground ,fg-normal))))
   `(whitespace-empty ((t (:background ,bg-normal :foreground ,fg-normal))))
   `(whitespace-line ((t (:background ,bg-normal :underline ,bg-off))))

   ;; Remaining Junk
   `(completion-dynamic-face ((t (:inherit 'match))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'devalot)
