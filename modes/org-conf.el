;;; org-conf.el -- Settings for `org' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-edna)
(require 'org-protocol)
(require 'ox-gfm)
(require 's)
(require 'warnings)

;; These autoloads are missing from their respective packages:
(autoload 'org-capture-ref-get-bibtex-field "org-capture-ref")
(autoload 'org-capture-ref-process-capture "org-capture-ref")
(autoload 'ox-ipynb-export-to-ipynb-buffer "ox-ipynb")

;; Silence compiler warnings
(declare-function consult-org-heading "consult")
(declare-function corg-setup "corg")
(declare-function org-appear-mode "org-appear")
(declare-function org-attach-attach "org-attach")
(declare-function org-attach-reveal-in-emacs "org-attach")
(declare-function org-attach-url "org-attach")
(declare-function org-bookmark-jump-unhide "org")
(declare-function org-bulletproof-mode "org-bulletproof")
(declare-function org-clock-dbus-mode "org-clock-dbus")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-clocking-p "org-clock")
(declare-function org-insert-last-stored-link "ol")
(declare-function org-modern-mode "org-modern")
(declare-function org-ref-insert-link "org-ref")
(declare-function org-roam-dailies-goto-date "org-roam")
(declare-function org-tree-slide-mode "org-tree-slide")
(declare-function pjones:delete-whitespace-mode "../lisp/whitespace.el")
(declare-function pjones:ensure-blank-lines "../lisp/interactive")
(declare-function pjones:open-line-above "../lisp/interactive")
(declare-function puni-mode "puni")
(declare-function yas-expand "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas-next-field "yasnippet")

(defvar org-attach-store-link-p)
(defvar org-clock-start-time)
(defvar pjones:current-theme)
(defvar yas/keymap)

(defvar pjones:org-notes-directory
  (expand-file-name "~/notes/")
  "Base directory where Org files are stored.")

(defvar pjones:org-publish-directory
  (expand-file-name "~/public/")
  "Directory where published files are stored.")

(defun pjones:org-agenda-files ()
  "Return a list of files that contain to-do items."
  (append
   (rx-let ((basename (+ (any alphanumeric))))
     (directory-files-recursively
      (concat pjones:org-notes-directory "gtd/")
      (rx string-start
          (or basename (seq basename ?/ basename))
          ?. "org" string-end)))
   (let ((today (calendar-current-date))
         files)
     (dotimes (n 3)
       (let* ((date (calendar-increment-month-cons
                     (* -1 n) (car today) (caddr today)))
              (path (format "%swiki/journal/%d/%02d.org"
                            pjones:org-notes-directory
                            (cdr date) (car date))))
         (when (file-exists-p path)
           (push path files))))
     files)))

(defun pjones:org-parse-effort-tag (tag)
  "Convert an effort TAG to a number of seconds."
  (if (string-match "^\\([0-9]+\\)\\([mh]\\)$" tag)
      (let ((num (string-to-number (match-string 1 tag)))
            (mod (match-string 2 tag)))
        (cond
         ((string= mod "m") (* num 60))
         ((string= mod "h") (* num 3600))
         (t num)))
    0))

(defun pjones:org-get-effort-tag-as-seconds (heading)
  "Return effort tags for HEADING converted to seconds."
  (-sum
   (-map #'pjones:org-parse-effort-tag
         (-filter
          (apply-partially #'string-match-p "^[0-9]")
          (get-text-property 1 'tags heading)))))

(defun pjones:org-sort-next-actions (a b)
  "Return sort order for to-do items A and B."
  (let ((ta (pjones:org-get-effort-tag-as-seconds a))
        (tb (pjones:org-get-effort-tag-as-seconds b)))
    (cond ((< ta tb) -1)
          ((< tb ta) +1))))

(defun pjones:org-time-stamp (&optional inactive time)
  "Return an `org-mode' timestamp.
If INACTIVE is non-nil, make the timestamp inactive.
If TIME is nil then use the current time."
  (let* ((style (cdr org-time-stamp-formats))
         (fmt (if inactive (concat "[" style "]")
                (concat "<" style ">"))))
    (format-time-string fmt time)))

(defvar pjones:org-template-jump-point nil
  "Where to jump after inserting a template.")

(defun pjones:org-template-insert (file)
  "Insert a template in the current tree.

If FILE is nil the template file name will be taken from the TEMPLATE
property in the current tree if that property exists.  Otherwise a file
name will be requested.  With a prefix argument the file name will
always be requested."
  (interactive (list (or (and (not current-prefix-arg)
                              (org-entry-get nil "TEMPLATE" t))
                         (read-file-name "Template: "))))
  (let* ((key "A")
         (id (org-entry-get nil "ID" t))
         (template `(,key "Anki Template" entry
                          ,(if id (list 'id id) '(here))
                          (file ,(if (file-name-absolute-p file) file
                                   (concat default-directory file)))
                          :empty-lines 1
                          :immediate-finish t
                          :jump-to-captured nil
                          :prepare-finalize
                          (lambda () (setq pjones:org-template-jump-point (point)))))
         (org-capture-templates (list template)))
    (undo-boundary)
    (org-capture nil key)
    (when pjones:org-template-jump-point
      (goto-char pjones:org-template-jump-point))))

;; General Org Settings
(custom-set-variables
 ;; Visual Settings:
 '(org-adapt-indentation t)
 '(org-agenda-breadcrumbs-separator " ❱ ")
 '(org-appear-autoentities t)
 '(org-appear-autokeywords t)
 '(org-appear-autolinks t)
 '(org-appear-autosubmarkers t)
 '(org-clock-clocked-in-display nil)
 '(org-clock-mode-line-total 'current)
 '(org-ellipsis " ")
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-modern-block-fringe nil)
 '(org-modern-block-name t)
 '(org-modern-hide-stars " ")
 '(org-modern-keyword nil)
 '(org-modern-tag nil)
 '(org-modern-timestamp nil)
 '(org-modern-todo nil)
 '(org-show-context-detail '((default . tree)))
 '(org-startup-folded 'show2levels)

 '(org-modern-fold-stars '(("⯈" . "⯆")
                           ("▶" . "▼")
                           ("▷" . "▽")
                           ("▸" . "▾")
                           ("▹" . "▿")))

 ;; Behavior Settings:
 '(org-archive-default-command #'pjones:org-archive-subtree-to-daily)
 '(org-archive-file-header-format nil)
 '(org-attach-archive-delete nil)
 '(org-attach-auto-tag nil)
 '(org-attach-dir-relative t)
 '(org-attach-id-dir (concat pjones:org-notes-directory "attachments/"))
 '(org-attach-method 'ln)
 '(org-attach-store-link-p 'attached)
 '(org-attach-use-inheritance t)
 '(org-auto-align-tags nil)
 '(org-blank-before-new-entry '((heading . t) (plain-list-item . t)))
 '(org-bulletproof-ordered-cycle '("1." "a."))
 '(org-catch-invisible-edits 'smart)
 '(org-clock-into-drawer t)
 '(org-columns-default-format "%60ITEM(Task) %EFFORT{:} %CLOCKED{:}")
 '(org-ctrl-k-protect-subtree nil)
 '(org-cycle-emulate-tab t)
 '(org-edit-src-persistent-message nil)
 '(org-fast-tag-selection-single-key nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-image-actual-width nil)
 '(org-imenu-depth 3)
 '(org-list-allow-alphabetical t)
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-outline-path-complete-in-steps nil)
 '(org-outline-path-complete-in-steps nil)
 '(org-reverse-note-order nil)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-src-window-setup 'current-window)
 '(org-tags-column 0)
 '(org-tags-exclude-from-inheritance nil)
 '(org-use-fast-tag-selection 'auto)
 '(org-use-fast-todo-selection 'expert)

 '(org-M-RET-may-split-line
   '((headline . nil)
     (item . nil)
     (default . t)))

 ;; Following Links
 '(org-file-apps
   '((auto-mode       . emacs)
     ("\\.docx\\'"    . "libreoffice %s")
     ("\\.m4v\\'"     . "vlc %s")
     ("\\.mkv\\'"     . "vlc %s")
     ("\\.mm\\'"      . default)
     ("\\.mp3\\'"     . "mpv %s")
     ("\\.mp4\\'"     . "vlc %s")
     ("\\.pages\\'"   . "libreoffice %s")
     ("\\.webm\\'"    . "vlc %s")
     ("\\.x?html?\\'" . default)
     ("\\.xlsx\\'"    . "libreoffice %s")))

 '(org-link-file-path-type 'relative)
 '(org-link-frame-setup
   '((file . find-file)
     (gnus . org-gnus-no-new-news)))

 ;; Tags:
 '(org-tag-persistent-alist
   '((:startgroup  . nil)
     ;; Places
     ("@home"      . ?h)
     ("@work"      . ?W)
     ("@out"       . ?o)
     ("@shanna"    . ?s)
     (:endgroup    . nil)
     (:startgroup  . nil)
     ;; Devices:
     ("@computer"  . ?c)
     ("@phone"     . ?P)
     ("@tablet"    . ?t)
     (:endgroup    . nil)
     (:startgroup  . nil)
     ;; Activities:
     ("@call"      . ?a)
     ("@email"     . ?e)
     ("@errand"    . ?E)
     ("@read"      . ?r)
     ("@plan"      . ?p)
     ("@write"     . ?w)
     ("@code"      . ?C)
     ("@labor"     . ?l)
     (:endgroup    . nil)
     (:startgroup  . nil)
     ;; Effort:
     ("5m"         . ?5)
     ("30m"        . ?3)
     ("1h"         . ?1)
     ("4h"         . ?4)
     (:endgroup    . nil)))

 ;; TODO keywords and faces:
 '(org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
     (sequence "NEXT(n)" "WAITING(w)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))

 '(org-todo-keyword-faces
   '(("NEXT"    . (:inherit font-lock-constant-face :weight bold))
     ("WAITING" . (:inherit font-lock-comment-face :weight bold))
     ("BLOCKED" . (:inherit org-agenda-dimmed-todo-face))))

 ;; Stuff for org-agenda.
 '(org-agenda-block-separator ?─)
 '(org-agenda-files (pjones:org-agenda-files))
 '(org-agenda-show-inherited-tags nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span 'day)
 '(org-agenda-start-day nil)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-time-leading-zero t)
 '(org-agenda-todo-ignore-deadlines 'near)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-agenda-todo-ignore-timestamp nil)
 '(org-agenda-todo-ignore-with-date nil)
 '(org-agenda-use-time-grid nil)
 '(org-agenda-window-setup 'current-window)
 '(org-deadline-warning-days 14)

 '(org-stuck-projects
   '("+project+LEVEL=2" ("NEXT" "WAITING" "BLOCKED") nil ""))

 `(org-agenda-custom-commands
   '(("c" "Current Status"
      ((agenda ""
        ((org-agenda-overriding-header "⚡ Agenda:")
         (org-agenda-remove-tags nil)
         (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
         (org-agenda-prefix-format "  %-12s %-12t %-8c ")
         (org-agenda-todo-keyword-format "")))
       (todo "WAITING"
        ((org-agenda-overriding-header "⚡ Waiting for Someone Else:")
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
         (org-agenda-remove-tags t)
         (org-agenda-prefix-format "  %-8c ")
         (org-agenda-todo-keyword-format "")))
       (tags-todo "@call|@email/NEXT"
         ((org-agenda-overriding-header "⚡ Phone Calls to Make, Emails to Send:")
          (org-agenda-prefix-format "  %-8c ")
          (org-agenda-remove-tags nil)
          (org-agenda-todo-keyword-format "")))
       (tags-todo "@read/NEXT"
         ((org-agenda-overriding-header "⚡ Reading and Research:")
          (org-agenda-prefix-format "  %-8c ")
          (org-agenda-remove-tags nil)
          (org-agenda-todo-keyword-format "")))
       (stuck ""
         ((org-agenda-overriding-header "⚡ Stuck Projects:")))
       (todo "BLOCKED"
         ((org-agenda-overriding-header "⚡ Missing Blocker Dependency:")
          (org-agenda-skip-function #'pjones:agenda-skip-properly-blocked)
          (org-agenda-remove-tags nil)
          (org-agenda-prefix-format "  %-8c ")
          (org-agenda-todo-keyword-format "")))
       (tags "+inbox+LEVEL=1"
         ((org-agenda-overriding-header "⚡ Inbox Tasks to Process:")
          (org-agenda-prefix-format "  %-8c ")
          (org-agenda-todo-keyword-format "")))
       (tags-todo "TODO=\"NEXT\"-SCHEDULED={.+}-DEADLINE={.+}-@call-@read-@email"
         ((org-agenda-overriding-header "⚡ Next Actions:")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
          (org-agenda-prefix-format "  %-8c ")
          (org-agenda-remove-tags nil)
          (org-agenda-todo-keyword-format "")
          (org-agenda-cmp-user-defined #'pjones:org-sort-next-actions)
          (org-agenda-sorting-strategy '(user-defined-up)))))
      nil (,(concat pjones:org-publish-directory "gtd/agenda.html")))
     ("p" "Project List"
      ((tags "+project+LEVEL=2")))
     ("T" "Travel Schedule"
      ((tags "+travel+TIMESTAMP>=\"<now>\""))
      ((org-agenda-view-columns-initially t)))))

 ;; Stuff for org-capture and org-refile:
 '(org-capture-bookmark nil)
 '(org-capture-ref-capture-template nil)
 '(org-capture-ref-headline-tags nil) ; Fix a bug in org-capture-ref
 '(org-default-notes-file (concat pjones:org-notes-directory "gtd/inbox.org"))
 '(org-log-refile 'time)
 '(org-refile-allow-creating-parent-nodes t)
 '(org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
 '(org-refile-use-outline-path t)

 '(org-capture-templates
   `(("i" "Capture to Inbox" entry
      (file ,org-default-notes-file)
      "* %?"
      :empty-lines 1)
     ("m" "Mail Message" entry
      (file ,org-default-notes-file)
      (file ,(concat pjones:org-notes-directory "templates/org/mail.org"))
      :empty-lines 1)
     ("p" "org-protocol-capture" entry
      (file ,org-default-notes-file)
      "* %:description\n\n  %:link\n\n  %i"
      :immediate-finish t
      :empty-lines 1)
     ("b" "Bibliography Link" entry
      (file+olp ,(concat pjones:org-notes-directory "bib/bibliography.org")
                "Inbox" "Read Next")
      (file ,(concat pjones:org-notes-directory "templates/org/bibliography.org"))
      :immediate-finish t
      :empty-lines 1)))

 ;; Preview control (more below):
 '(org-preview-latex-default-process 'dvisvgm)

 ;; Stuff for exporting:
 '(org-babel-results-keyword "results")
 '(org-export-exclude-tags '("noexport" "wikionly"))
 '(org-export-with-smart-quotes t)
 '(org-highlight-latex-and-related '(native))
 '(org-html-htmlize-output-type 'css)
 '(org-html-validation-link nil)
 '(org-icalendar-include-todo t)
 '(org-latex-compiler "xelatex")
 '(org-latex-listings 'minted)
 '(org-latex-prefer-user-labels t)
 '(org-latex-tables-booktabs t)
 '(org-beamer-frame-level 2)
 '(org-plantuml-exec-mode 'plantuml)

 '(org-babel-default-header-args
   '((:cache   . "no")
     (:eval    . "never-export")
     (:exports . "both")
     (:hlines  . "no")
     (:noweb   . "no")
     (:results . "replace")
     (:session . "none")
     (:tangle  . "no")))

 '(org-babel-default-header-args:plantuml
   '((:eval    . "yes")
     (:exports . "results")
     (:results . "file graphics")))

 '(org-cite-export-processors
   '((latex . (biblatex "apa" nil))
     (t     . (basic "numeric" "numeric"))))

 '(org-cite-global-bibliography
   (list (concat pjones:org-notes-directory "bib/bibliography.bib")))

 '(org-format-latex-options
   '(:foreground default
     :background default
     :scale 1.0
     :html-foreground "Black"
     :html-background "Transparent"
     :html-scale 1.0
     :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

 '(org-latex-pdf-process
   '("latexmk -xelatex -pdfxe -shell-escape %f"))

 '(org-latex-toc-command
   (string-join
    '("{"
      "\\hypersetup{linkcolor=black}"
      "\\tableofcontents"
      "}\n")
    "\n"))

 '(org-latex-packages-alist
   '(("" "booktabs")
     ("" "color")
     ("" "fontspec")
     "\\setmainfont{FreeSerif}"
     ("newfloat" "minted")
     ("" "svg")
     ("" "transparent")
     ("" "xcolor")))

 '(org-latex-with-hyperref
   "\\hypersetup{
      pdfauthor={%a},
      pdftitle={%t},
      pdfkeywords={%k},
      pdfsubject={%d},
      pdfcreator={%c},
      pdflang={%L},
      citecolor=blue,
      colorlinks=true,
      linkcolor=blue,
      urlcolor=blue\n}\n")

 `(org-publish-project-alist
   '(("gtd"
      :base-directory ,(concat pjones:org-notes-directory "gtd/")
      :base-extension "org"
      :recursive t
      :exclude "archive\\.org$"
      :publishing-function org-html-publish-to-html
      :publishing-directory ,(concat pjones:org-publish-directory "gtd/")
      :with-author nil
      :with-date nil
      :section-numbers t
      :with-broken-links t
      :with-toc 2
      :archived-trees nil
      :html-postamble nil
      :html-link-home "../wiki/index.html"
      :html-link-up "../wiki/sitemap.html"
      :html-home/up-format
      "<div id=\"org-div-home-and-up\">
      <a title=\"Topics\" href=\"%s\">🌎</a>
      <a title=\"Home\" href=\"%s\">🏠</a>
      </div>")
     ("bib"
      :base-directory ,(concat pjones:org-notes-directory "bib/")
      :base-extension "org"
      :publishing-function org-html-publish-to-html
      :publishing-directory ,(concat pjones:org-publish-directory "bib/")
      :with-author nil
      :with-date nil
      :section-numbers t
      :with-broken-links t
      :with-toc 2
      :archived-trees nil
      :html-postamble nil
      :html-link-home "../wiki/index.html"
      :html-link-up "../wiki/sitemap.html"
      :html-home/up-format
      "<div id=\"org-div-home-and-up\">
      <a title=\"Topics\" href=\"%s\">🌎</a>
      <a title=\"Home\" href=\"%s\">🏠</a>
      </div>")
     ("bib-static"
      :base-directory ,(concat pjones:org-notes-directory "bib/")
      :base-extension "bib"
      :publishing-directory ,(concat pjones:org-publish-directory "bib/")
      :publishing-function org-publish-attachment)
     ("wiki"
      :base-directory ,(concat pjones:org-notes-directory "wiki/")
      :base-extension "org"
      :recursive t
      :auto-sitemap t
      :sitemap-title "Peter's Knowledge Base (All Pages)"
      :sitemap-filename ,(concat pjones:org-publish-directory "wiki/sitemap.org")
      :sitemap-function pjones:org-publish-sitemap
      :sitemap-sort-folders ignore
      :sitemap-style list ; Tree is broken :(
      :preparation-function pjones:org-roam-before-publish
      :completion-function pjones:org-roam-after-publish
      :publishing-function org-html-publish-to-html
      :publishing-directory ,(concat pjones:org-publish-directory "wiki/")
      :with-author nil
      :with-date nil
      :section-numbers t
      :with-broken-links t
      :with-toc nil
      :html-postamble nil
      :html-link-home "../../../index.html"
      :html-link-up "../../../sitemap.html"
      :html-home/up-format
      "<div id=\"org-div-home-and-up\">
      <a title=\"Topics\" href=\"%s\">🌎</a>
      <a title=\"Home\" href=\"%s\">🏠</a>
      </div>")
     ("attachments"
      :base-directory ,(concat pjones:org-notes-directory "attachments/")
      :base-extension 'any
      :recursive t
      :publishing-directory ,(concat pjones:org-publish-directory "attachments/")
      :publishing-function org-publish-attachment)
     ("notes"
      :components ("wiki"
                   "gtd"
                   "bib"
                   "bib-static"
                   "attachments")))))

;; Custom LaTeX classes:
(setq org-latex-classes
      (cl-remove-if
       (lambda (entry) (string-match-p "^pjones-" (car entry)))
       org-latex-classes))

;; Basic support for the letter class:
(add-to-list 'org-latex-classes
             '("letter"
               "\\documentclass{letter}\n\\newcommand\\maketitle{}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Correctly generate LaTeX previews:
(plist-put
 (cdr (assq 'dvisvgm org-preview-latex-process-alist))
 :latex-compiler '("lualatex --output-format=dvi --interaction nonstopmode --output-directory %o %f"))

(custom-set-faces
 '(org-block ((t (:background nil))))
 '(org-block-begin-line ((t (:background nil)))))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((shell . t)
      (emacs-lisp . t)
      (jq . t)
      (mermaid . t)
      (plantuml . t)
      (python . t)
      (R . t)
      (ruby . t)))

;; Fucking `org-element' constant bugs:
(push '(org-element-cache) warning-suppress-types)
(push '(org-element) warning-suppress-types)

(defun pjones:org-capture-ref-bibtex ()
  "Return the BibTex string for use in a source block."
  (let ((bibtex (org-capture-ref-get-bibtex-field :bibtex-string)))
    (s-replace-regexp "^" "     " (or bibtex "MISSING") t)))

(defun pjones:org-agenda-mode-hook ()
  "Hook run after a `org-agenda-mode' buffer is created."
  (hl-line-mode 1))

(defun pjones:org-hide-others ()
  "Close all headings except the heading at point."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-overview))
  (org-fold-show-context 'agenda)
  (org-fold-show-children)
  (org-fold-show-entry))

(defun pjones:org-effort-sum (&optional skip-done clock-diff)
  "Recursively sum the Effort property.
If SKIP-DONE is non-nil done headings report an effort of 0.  If
CLOCK-DIFF is non-nil, return the difference between the effort and
clocked time."
  (apply '+ (org-map-entries
    (lambda ()
      (if (and skip-done (org-entry-is-done-p)) 0
        (let* ((effort (org-entry-get (point) "Effort"))
               (mins (if effort (org-duration-to-minutes effort) 0)))
          (if (and (> mins 0) clock-diff)
              (- mins (org-clock-sum-current-item))
            mins))))
    t 'tree)))

(defun org-dblock-write:pjones-project-review (params)
  "Create a table for reviewing projects.

PARAMS is a property list of parameters:

`:id' (mandatory) The heading to start at.
`:hlines' (optional number) Draw a line before this heading level."
  (let ((id (or (plist-get params :id) (error "Missing :id")))
        (hlines (or (plist-get params :hlines) 0))
        base-level table)
    (insert "| Task | Effort | Clocked | Remaining |\n")
    (insert "|------|--------|---------|-----------|\n")
    (save-excursion
      (let ((m (org-id-find id 'marker)))
        (unless m
          (error "Cannot find entry with ID \"%s\"" id))
        (with-current-buffer (marker-buffer m)
          (goto-char m)
          (org-fold-show-context)
          (setq base-level (org-current-level))
          (org-map-entries
           (lambda ()
             (let* ((level (- (org-current-level) base-level))
                    (indent (if (> level 0) (concat "\\_" (make-string level ?_))))
                    (title (concat indent " " (org-get-heading)))
                    (clock (org-clock-sum-current-item))
                    (effort (pjones:org-effort-sum))
                    (left (pjones:org-effort-sum t t))
                    plist)
               (setq plist (list
                 :title title
                 :level (org-current-level)
                 :effort (org-duration-from-minutes effort)
                 :clock (org-duration-from-minutes clock)
                 :left (org-duration-from-minutes left)))
               (push plist table)))
           t 'tree))))
      (dolist (row (nreverse table))
        (when (= hlines (plist-get row :level))
          (insert "|-\n"))
        (dolist (attr (list :title :effort :clock :left))
          (insert (concat "| " (plist-get row attr))))
        (insert "|\n"))
      (org-table-align)))

(defun pjones:org-up-or-prev (&optional arg)
  "Move to the parent, or previous sibling.
ARG is the number of headings to move."
  (interactive "p")
   (if (= 1 (org-outline-level))
       (org-backward-heading-same-level arg)
     (outline-up-heading arg)))

(defun pjones:org-backward-heading-same-level (&optional arg)
  "Move backard to the preceding headline.
ARG is the number of headings to move."
  (interactive "p")
  (if (org-at-heading-p)
      (org-backward-heading-same-level arg)
    (org-back-to-heading)
    (org-backward-heading-same-level (- arg 1))))

(defun pjones:org-next-item ()
  "Move to the next plain item.
This is a wrapper around `org-next-item'.  That function doesn't work if
you are not already in a plain list."
  (interactive)
  (require 'org-list)
  (if (org-in-item-p) (org-next-item)
    (org-list-search-forward (org-item-beginning-re))))

(defun pjones:org-insert-heading (&optional here)
  "Insert a heading sanely.
When HERE is non-nil, create a heading after point."
  (interactive "P")
  (let ((org-insert-heading-respect-content
         (not (or here (and (org-at-heading-p) (bolp)))))
        (insert-fn (lambda (mark-todo)
                     (if mark-todo
                         (org-insert-todo-heading '(4) t)
                       (org-insert-heading)))))
    (if (or here (org-at-heading-p))
        (funcall insert-fn (org-get-todo-state))
      (org-back-to-heading)
      (let ((mark-todo (org-get-todo-state)))
        (end-of-line)
        (funcall insert-fn mark-todo)))
    (when (org--blank-before-heading-p)
      (pjones:ensure-blank-lines))))

(defun pjones:org-insert-item (checkbox)
  "Insert a new item.
If CHECKBOX is non-nil, add a checkbox too.  If called from a list
item that already has a checkbox, then CHECKBOX means the opposite.

This replaces `org-insert-item' which doesn't work unless there's an
existing item.  This version works on headings too."
  (interactive "P")
  (save-excursion
    (when (not (org-at-item-p))
      (org-backward-element))
    (when (org-at-item-checkbox-p)
      (setq checkbox (not checkbox))))
  (unless (org-insert-item checkbox)
    (org-back-to-heading)
    (org-fold-show-subtree)
    (let ((here (point)))
      (outline-next-heading)
      (forward-line 0)
      (if (/= here (point))
          (pjones:open-line-above nil)
        (forward-line)
        (condition-case nil
            (while t (org-forward-element))
          (user-error nil))
        (end-of-line)
        (newline))
      (indent-according-to-mode)
      (insert "- ")
      (when checkbox (insert "[ ] "))))
  (pjones:ensure-blank-lines))

(defun pjones:org-open-line (arg)
  "Open a line, the correct way.
If ARG is non-nil then open below instead of above.  Like the original
version, properly handles tables."
  (interactive "P")
  (if (org-at-table-p) (org-table-insert-row arg)
    (pjones:open-line-above arg)))

(defun pjones:org-archive-subtree-to-daily (&optional _find-done)
  "Archive the current subtree to the roam daily file."
  (interactive)
  (require 'org-roam)
  (when-let* ((today (save-excursion
                       (org-roam-dailies-goto-date nil "a")
                       (buffer-file-name)))
              (org-archive-location
               (concat today "::* Archive")))
    (org-archive-subtree 0)))

(defun pjones:org-attach (file)
  "Attach a FILE then insert link to it."
  (interactive "f")
  (org-attach-attach file)
  (when org-attach-store-link-p
    (org-insert-link nil (caar org-stored-links))))

;; Allow DIR property to be relative to org-attach-id-dir
(defun pjones:org-attach-dir (&optional create-if-not-exists-p no-fs-check)
  "Return the directory associated with the current outline node.
First check for DIR property, then ID property.
`org-attach-use-inheritance' determines whether inherited
properties also will be considered.

If an ID property is found the default mechanism using that ID
will be invoked to access the directory for the current entry.
Note that this method returns the directory as declared by ID or
DIR even if the directory doesn't exist in the filesystem.

If CREATE-IF-NOT-EXISTS-P is non-nil, `org-attach-dir-get-create'
is run.  If NO-FS-CHECK is non-nil, the function returns the path
to the attachment even if it has not yet been initialized in the
filesystem.

If no attachment directory can be derived, return nil."
  (let (attach-dir id)
    (cond
     (create-if-not-exists-p
      (setq attach-dir (org-attach-dir-get-create)))
     ((setq attach-dir (org-entry-get nil "DIR" org-attach-use-inheritance))
      (org-attach-check-absolute-path attach-dir)
      (unless (file-name-absolute-p attach-dir)
        (setq attach-dir (file-name-concat (or org-attach-id-dir
                                               default-directory)
                                           attach-dir))))
     ;; Deprecated and removed from documentation, but still
     ;; works. FIXME: Remove after major nr change.
     ((setq attach-dir (org-entry-get nil "ATTACH_DIR" org-attach-use-inheritance))
      (org-attach-check-absolute-path attach-dir))
     ((setq id (org-entry-get nil "ID" org-attach-use-inheritance))
      (org-attach-check-absolute-path nil)
      (setq attach-dir (org-attach-dir-from-id id 'existing))))
    (if no-fs-check
	attach-dir
      (when (and attach-dir (file-directory-p attach-dir))
	attach-dir))))

(advice-add #'org-attach-dir :override #'pjones:org-attach-dir)

(defun pjones:org-promote-demote (promote)
  "Promote or demote the current heading or item.
PROMOTE should be non-nil to promote, or nil to demote."
  (when-let ((fun (cond
                   ((org-at-heading-p)
                    (if promote #'org-promote-subtree
                      #'org-demote-subtree))
                   ((org-at-item-p)
                    (if promote #'org-outdent-item-tree
                      #'org-indent-item-tree)))))
    (call-interactively fun)))

(defun pjones:org-promote nil
  "Promote the current heading or item."
  (interactive)
  (pjones:org-promote-demote t))

(defun pjones:org-demote nil
  "Demote the current heading or item."
  (interactive)
  (pjones:org-promote-demote nil))

(defun pjones:agenda-skip-properly-blocked ()
  "Skip a blocked entry if it has a proper blocker.
Meant to be used with `org-agenda-skip-function'."
  (org-back-to-heading t)
  (when (string= "BLOCKED" (org-get-todo-state))
    (let* ((end (org-entry-end-position))
           (pom (point))
           (form (org-entry-get pom "BLOCKER" nil)))
      (and (org-edna-process-form form 'condition) end))))

;; https://lists.gnu.org/archive/html/emacs-orgmode/2015-06/msg00266.html
(defun pjones:org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
A block is identified as empty if there are fewer than 2
non-empty lines in the block (excluding the line with
`org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (pos-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (pos-eol))))))
  (setq buffer-read-only t))

;; From Consult:
(defvar consult--customize-alist)

(defun pjones:org-get-id (&optional prompt)
  "Navigate to a heading and return its ID.
If called interactively, also put the ID on the kill ring.

If PROMPT is set, use that as the consult prompt."
  (interactive)
  (let* ((prompt (or prompt "Get ID for heading: "))
         (consult--customize-alist
          `((,this-command :prompt ,prompt)))
         (id (save-mark-and-excursion
               (deactivate-mark)
               (if (fboundp 'consult-org-heading)
                   (consult-org-heading)
                 (org-goto))
               (org-id-get nil t))))
    (prog1 (identity id)
      (when (called-interactively-p 'any)
        (kill-new id)))))

(defun pjones:org-insert-heading-link ()
  "Prompt for a heading and insert a link to it."
  (interactive)
  (let ((id (pjones:org-get-id "Insert link for heading: ")))
    (funcall-interactively 'org-insert-link nil (concat "id:" id))))

(defun pjones:after-org-edna-edit ()
  "Put each blocker and trigger on its own line."
  (save-excursion
    (goto-char org-edna-blocker-section-marker)
    (while (re-search-forward ") +" nil t)
      (replace-match ")\n" nil nil))))

(advice-add 'org-edna-edit :after 'pjones:after-org-edna-edit)

(defvar pjones:org-todo-state-after-block "DONE"
  "The state to move a to-do item after it is unblocked.")

(defun pjones:org-todo-block (&optional edit)
  "Mark the current heading as blocked.
Prompts for a target heading that is blocking the current heading
and properly sets up the BLOCKER and TRIGGER properties.

If EDIT is non-nil then edit the resulting trigger with
`org-edna-edit' even if no trigger currently exists."
  (interactive "P")
  (let* ((heading-point (save-excursion
                          (org-back-to-heading)
                          (point-marker)))
         (source (org-id-get heading-point t))
         (target (pjones:org-get-id "Heading on which to block: "))
         (blocker (or (org-entry-get heading-point "BLOCKER") ""))
         (trigger (save-excursion
                    (org-id-goto target)
                    (or (org-entry-get (point-marker) "TRIGGER") ""))))
    (org-todo "BLOCKED")
    (org-entry-put heading-point "BLOCKER"
                   (string-join
                    (remove nil (list (format "ids(id:%s)" target) blocker))
                    " "))
    (if (and (not edit) (string-empty-p trigger))
        (save-excursion
          (org-id-goto target)
          (org-entry-put (point-marker) "TRIGGER"
                         (format "ids(id:%s) todo!(%s)"
                                 source
                                 pjones:org-todo-state-after-block)))
      (org-id-goto target)
      (org-edna-edit)
      (goto-char org-edna-blocker-section-marker)
      (move-beginning-of-line nil)
      (open-line 2)
      (insert "The first trigger references the ID of the blocked heading.")
      (goto-char org-edna-trigger-section-marker)
      (move-end-of-line nil)
      (newline)
      (insert (format "ids(id:%s)\n" source)))))

(defun pjones:ox-ipynb-export-to-ipynb ()
  "Export to a Juypter notebook."
  (interactive)
  (save-excursion
    (when-let* ((buffer (ox-ipynb-export-to-ipynb-buffer))
                (file (buffer-local-value 'export-file-name buffer)))
      (with-current-buffer buffer
        (write-file file))
      (kill-buffer buffer))))

(defun pjones:org-check-links ()
  "Ensure file links are valid."
  (interactive)
  (let ((pos (point)))
    (goto-char (point-min))
    (while (re-search-forward org-link-any-re nil t)
      (when-let ((link (save-excursion
                         (goto-char (match-beginning 0))
                         (org-element-link-parser))))
        (pcase (org-element-property :type link)
          ("file"
           (unless (file-exists-p (org-element-property :path link))
             (goto-char (org-element-begin link))
             (when (org-invisible-p) (org-fold-show-context 'link-search))
             (error "The link at point doesn't exit"))))))
    (goto-char pos)
    (message "All links are good.")))

(defun pjones:org-before-beamer-export (backend)
  "Prepare buffer for export to beamer.

Only works with BACKEND is \='beamer.

Called from `org-export-before-processing-functions'."
  (when-let (((eq backend 'beamer))
             (keywords '(("beamer_class_options"    . "latex_class_options")
                         ("beamer_options"          . "options")
                         ("beamer_exclude_tags"     . "exclude_tags")
                         ("beamer_export_file_name" . "export_file_name"))))
    (pjones:org-translate-keywords keywords)))

(defun pjones:org-translate-keywords (keywords)
  "Replace keywords in the current buffer according to KEYWORDS.

KEYWORDS should be an alist of strings where the keys are keywords to
match and the values are replacement keywords."
  (let* ((quoted (mapcar (lambda (e) (regexp-quote (car e))) keywords))
         (regexp (concat "^[[:blank:]]*#\\+\\("
                         (string-join quoted "\\|")
                         "\\):"))
         (case-fold-search t))
    (undo-boundary)
    (save-excursion
      (goto-char (point-min))
      (while-let (((search-forward-regexp regexp nil t))
                  (key (match-string-no-properties 1))
                  (val (alist-get key keywords nil nil #'string=)))
        (replace-match val nil t nil 1)
        (forward-line)))))

(defun pjones:org-latex-export-to-pdf (async)
  "Smart (DWIM) export to PDF.
When ASYNC is non-nil then export in the background."
  (interactive "P")
  (message "Generating PDF...")
  (let ((only-subtree (buffer-narrowed-p)))
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf async only-subtree))))

;;; Key Bindings:
(let ((map org-mode-map))
  ;; Reset these so I can use them as a prefix:
  (define-key map (kbd "C-c C-a") nil)
  (define-key map (kbd "C-c C-e") nil)

  (define-key map (kbd "<f12>") #'org-tree-slide-mode)
  (define-key map (kbd "C-'") nil) ; Remove this binding.
  (define-key map (kbd "C-<return>") #'pjones:org-insert-heading)
  (define-key map (kbd "C-c C-a a") #'pjones:org-attach)
  (define-key map (kbd "C-c C-a d") #'org-attach-reveal-in-emacs)
  (define-key map (kbd "C-c C-a u") #'org-attach-url)
  (define-key map (kbd "C-c i") #'pjones:org-template-insert)
  (define-key map (kbd "C-c C-b") #'pjones:org-todo-block)
  (define-key map (kbd "C-c C-e b") #'org-beamer-export-to-pdf)
  (define-key map (kbd "C-c C-e e") #'org-export-dispatch)
  (define-key map (kbd "C-c C-e j") #'pjones:ox-ipynb-export-to-ipynb)
  (define-key map (kbd "C-c C-e m") #'org-gfm-export-as-markdown)
  (define-key map (kbd "C-c C-e p") #'pjones:org-latex-export-to-pdf)
  (define-key map (kbd "C-c C-x @") #'org-ref-insert-link)
  (define-key map (kbd "C-c l h") #'pjones:org-insert-heading-link)
  (define-key map (kbd "C-c RET") nil) ; Remove this binding.
  (define-key map (kbd "C-M-n") #'org-next-visible-heading)
  (define-key map (kbd "C-M-p") #'pjones:org-up-or-prev)
  (define-key map (kbd "C-o") #'pjones:org-open-line)
  (define-key map (kbd "M-<left>") #'pjones:org-promote)
  (define-key map (kbd "M-<return>") #'pjones:org-insert-item)
  (define-key map (kbd "M-<right>") #'pjones:org-demote)
  (define-key map (kbd "M-g C-i") #'pjones:org-get-id)
  (define-key map (kbd "M-g i") #'consult-org-heading)
  (define-key map (kbd "M-n") #'org-forward-heading-same-level)
  (define-key map (kbd "M-N") #'pjones:org-next-item)
  (define-key map (kbd "M-p") #'pjones:org-backward-heading-same-level)
  (define-key map (kbd "M-P") #'org-previous-item))

(defmacro pjones:org-eval-in-calendar (function)
  "Generate a command to call FUNCTION from within `org-eval-in-calendar'."
  `(defun ,(intern (concat "pjones:org-eval-in-calednar-" (symbol-name function))) ()
    (interactive)
    (org-eval-in-calendar '(,function 1))))

(let ((map org-read-date-minibuffer-local-map))
  (define-key map (kbd "M-b") (pjones:org-eval-in-calendar calendar-backward-day))
  (define-key map (kbd "M-f") (pjones:org-eval-in-calendar calendar-forward-day))
  (define-key map (kbd "M-p") (pjones:org-eval-in-calendar calendar-backward-week))
  (define-key map (kbd "M-n") (pjones:org-eval-in-calendar calendar-forward-week)))

(defvar-keymap org-mode-repeat-map
  :repeat t
  "d" #'org-next-visible-heading
  "n" #'org-forward-heading-same-level
  "p" #'pjones:org-backward-heading-same-level
  "u" #'pjones:org-up-or-prev)

(defvar-keymap pjones:org-mode-map
  :doc "Access frequently used `org-mode' functions."
  "a" #'org-fold-show-subtree
  "b" #'org-fold-show-branches
  "h" #'org-fold-hide-sublevels
  "k" #'org-ctrl-c-ctrl-c
  "o" #'pjones:org-hide-others
  "s" #'org-toggle-narrow-to-subtree)

(defun pjones:org-mode-hook ()
  "Hook to hack `org-mode'."
  (unless noninteractive
    ;; Orgzly insists on inserting empty lines at the end of files.  So
    ;; if they are removed in Emacs they will cause a sync conflict or
    ;; just come back on their own.  So, don't delete them.
    (setq-local delete-trailing-lines nil)
    (pjones:delete-whitespace-mode)

    ;; Modes to turn off:
    (save-place-mode -1)                ; Don't jump to hidden places
    (puni-mode -1)                      ; Puni doesn't work here

    ;; Modes to turn on:
    (org-appear-mode)
    (org-bulletproof-mode)
    (org-clock-dbus-mode)
    (org-edna-mode)
    (org-modern-mode)

    ;; Better src block completion:
    (require 'corg)
    (corg-setup)

    ;; Use yasnippets:
    (yas-minor-mode)
    (setq-local yas/trigger-key [tab])
    (define-key yas/keymap [tab] #'yas-next-field)
    (add-to-list 'org-tab-first-hook #'yas-expand)

    ;; Install custom key bindings:
    (keymap-local-set "C-c k" pjones:org-mode-map)))

;;; Hooks
(add-hook 'org-agenda-after-show-hook #'pjones:org-hide-others)
(add-hook 'org-agenda-finalize-hook #'pjones:org-agenda-delete-empty-blocks)
(add-hook 'org-agenda-mode-hook #'pjones:org-agenda-mode-hook)
(add-hook 'org-export-before-processing-functions #'pjones:org-before-beamer-export)
(add-hook 'org-mode-hook #'pjones:org-mode-hook)

;;; org-conf.el ends here

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
