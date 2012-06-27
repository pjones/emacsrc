;;; devalot-theme.el --- Solarized-based custom theme for faces

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Authors: Peter Jones <pjones@pmade.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; The majority of the colors in this theme come from the Solarized
;; palette, which is released under an MIT license:
;; https://github.com/altercation/solarized.
;;; Code:

(deftheme devalot "A theme loosely based on solarized.")

;; Notes
;; * To see all faces in effect: list-faces-display
(let ((yellow     "#b58900")
      (orange     "#cb4b16")
      (red        "#dc322f")
      (magenta    "#d33682")
      (violet     "#6c71c4")
      (blue       "#268bd2")
      (cyan       "#2aa198")
      (green      "#859900")
      (bg-normal  "#222222")
      (bg-off     "#003340")
      (bg-high    "#083d4a")
      (bg-low      "#2a2a2a")
      (bg-inverse "#fdf6e3")
      (fg-normal  "#839496")
      (fg-low     "#586e75")
      (fg-high    "#93a1a1")
      (fg-inverse "#657b83")
      (class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'devalot
   `(default        ((((type graphic)) (:background ,bg-normal :foreground ,fg-normal))))
   `(cursor         ((t (:background ,yellow :foreground ,bg-normal))))
   `(link           ((t (:underline  ,cyan))))
   `(match          ((t (:background ,bg-high :foreground ,fg-normal))))
   `(fringe         ((t (:background ,bg-low :foreground ,fg-low))))
   `(lazy-highlight ((t (:background ,bg-inverse :foreground ,fg-inverse))))
   `(isearch        ((t (:background ,magenta :foreground ,bg-inverse))))
   `(region         ((t (:background ,bg-high))))
   `(highlight      ((t (:background ,green :foreground ,bg-inverse))))
   `(hl-line        ((t (:background ,green :foreground ,bg-inverse))))

   ;; Show paren
   `(show-paren-match ((t (:background ,bg-inverse
                                       :foreground ,blue
                                       :bold t))))

   ;; Font-lock
   `(font-lock-builtin-face           ((t (:foreground ,fg-low))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-inverse :bold t))))
   `(font-lock-comment-face           ((t (:foreground ,violet :italic t))))
   `(font-lock-constant-face          ((t (:foreground ,yellow))))
   `(font-lock-function-name-face     ((t (:foreground ,orange :bold t))))
   `(font-lock-keyword-face           ((t (:foreground ,green :bold t))))
   `(font-lock-preprocessor-face      ((t (:foreground ,red))))
   `(font-lock-string-face            ((t (:foreground ,cyan))))
   `(font-lock-type-face              ((t (:foreground ,blue :bold t))))
   `(font-lock-variable-name-face     ((t (:foreground ,magenta))))
   `(font-lock-warning-face           ((t (:underline  ,red))))

   ;; Minibuffer
   `(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))

   ;; Modeline and Things in the Modeline
   `(modeline ((t (:background ,yellow :foreground ,bg-normal :box (:line-width 1 :style released-button)))))
   `(mode-line-inactive ((t (:background ,bg-high :foreground ,fg-normal :box (:line-width 1 :style released-button)))))
   `(modeline-mousable ((t (:background ,bg-high :foreground ,fg-normal))))
   `(modeline-mousable-minor-mode ((t (:background ,bg-high :foreground ,bg-high))))

   ;; Flyspell
   `(flyspell-duplicate ((t (:underline ,yellow))))
   `(flyspell-incorrect ((t (:underline ,red))))

   ;; Outline Mode
   `(outline-1 ((t (:foreground ,blue    :bold t))))
   `(outline-2 ((t (:foreground ,violet  :bold t))))
   `(outline-3 ((t (:foreground ,magenta :bold nil))))
   `(outline-4 ((t (:foreground ,yellow  :bold nil))))
   `(outline-5 ((t (:foreground ,cyan    :bold nil))))

   ;; Org-Mode
   `(org-level-1               ((t (:inherit 'outline-1))))
   `(org-level-2               ((t (:inherit 'outline-2))))
   `(org-level-3               ((t (:inherit 'outline-3))))
   `(org-level-4               ((t (:inherit 'outline-4))))
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
   `(org-special-keyword       ((t (:inherit 'org-tag))))
   `(org-code                  ((t (:inherit 'font-lock-function-name-face :bold nil))))
   `(org-hide                  ((t (:foreground ,bg-off))))
   `(org-column                ((t (:background ,bg-high))))
   `(org-column-title          ((t (:inherit 'mode-line :background ,orange))))
   `(org-checkbox              ((t (:inherit 'mode-line :background ,green))))
   `(org-todo                  ((t (:inherit 'mode-line :background ,red))))
   `(org-done                  ((t (:inherit 'mode-line :background ,green))))

   ;; ERB (Ruby Embedded in HTML)
   `(erb-face               ((t (:inherit 'default))))
   `(erb-delim-face         ((t (:foreground ,fg-low))))
   `(erb-out-face           ((t (:inherit 'default))))
   `(erb-out-delim-face     ((t (:foreground ,blue))))
   `(erb-comment-delim-face ((t (:inherit 'font-lock-comment-delimiter-face))))
   `(erb-comment-face       ((t (:inherit 'font-lock-comment-face))))

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
   `(flymake-errline ((t :underline ,red)))
   `(flymake-warnline ((t :underline ,yellow)))

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
   `(ido-first-match ((t (:inherit 'font-lock-string-face))))
   `(ido-subdir      ((t (:inherit 'font-lock-function-name-face))))

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
   `(whitespace-tab ((,class (:background ,bg-low))))
   `(whitespace-trailing ((,class (:background ,bg-normal :foreground ,fg-normal))))
   `(whitespace-empty ((,class (:background ,bg-normal :foreground ,fg-normal))))
   `(whitespace-line ((,class (:background ,bg-low))))

   ;; Non-Standard Faces
   ;;`(devalot-fixme-face ((t (:background ,bg-inverse :foreground ,red :bold t :box (:line-width 1 :color ,bg-low)))))

   ;; Remaining Junk
   `(completion-dynamic-face ((t (:inherit 'match))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'devalot)
