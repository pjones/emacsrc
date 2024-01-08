;;; org-conf.el -- Settings for org-mode.
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'ox-gfm)
(require 's)
(require 'warnings)

;; Silence compiler warnings
(declare-function consult-org-heading "consult")
(declare-function dbus-send-signal "dbus")
(declare-function org-appear-mode "org-appear")
(declare-function org-attach-attach "org-attach")
(declare-function org-attach-reveal-in-emacs "org-attach")
(declare-function org-attach-url "org-attach")
(declare-function org-bookmark-jump-unhide "org")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-clocking-p "org-clock")
(declare-function org-insert-last-stored-link "ol")
(declare-function org-roam-dailies-goto-date "org-roam")
(declare-function org-tree-slide-mode "org-tree-slide")
(declare-function pjones:ensure-blank-lines "../lisp/interactive")
(declare-function pjones:open-line-above "../lisp/interactive")
(declare-function puni-mode "puni")
(declare-function whitespace-mode "whitespace")
(declare-function yas-expand "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas-next-field "yasnippet")

(defvar dbus-interface-emacs)
(defvar dbus-path-emacs)
(defvar org-attach-store-link-p)
(defvar org-clock-start-time)
(defvar whitespace-style)
(defvar yas/keymap)

(defvar pjones:org-notes-directory
  (expand-file-name "~/notes/")
  "Base directory where Org files are stored.")

(defvar pjones:org-publish-directory
  (expand-file-name "~/public/")
  "Directory where published files are stored.")

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

;; General Org Settings
(custom-set-variables
 ;; Visual Settings:
 '(org-hide-leading-stars t)
 '(org-ellipsis "…")
 '(org-clock-clocked-in-display nil)
 '(org-show-context-detail (quote ((default . tree))))
 '(org-duration-format (quote h:mm))
 '(org-hide-emphasis-markers t)
 '(org-adapt-indentation t)
 '(org-appear-autolinks nil)
 '(org-appear-autosubmarkers t)
 '(org-appear-autoentities t)
 '(org-appear-autokeywords t)

 ;; Behavior Settings:
 '(org-blank-before-new-entry '((heading . t) (plain-list-item . t)))
 '(org-catch-invisible-edits 'smart)
 '(org-log-done 'time)
 '(org-reverse-note-order nil)
 '(org-tags-column 0)
 '(org-use-fast-todo-selection 'expert)
 '(org-use-fast-tag-selection (quote auto))
 '(org-fast-tag-selection-single-key nil)
 '(org-imenu-depth 3)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-clock-into-drawer t)
 '(org-log-into-drawer t)
 '(org-tags-exclude-from-inheritance nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-outline-path-complete-in-steps nil)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-edit-src-persistent-message nil)
 '(org-src-window-setup (quote current-window))
 '(org-attach-id-dir (concat pjones:org-notes-directory "attachments/"))
 '(org-attach-auto-tag nil)
 '(org-attach-dir-relative t)
 '(org-attach-method 'ln)
 '(org-attach-store-link-p 'attached)
 '(org-attach-archive-delete nil)
 '(org-attach-use-inheritance t)
 '(org-capture-bookmark nil)
 '(org-archive-file-header-format nil)
 '(org-archive-default-command #'pjones:org-archive-subtree-to-daily)
 '(org-M-RET-may-split-line
   '((headline . nil)
     (item . nil)
     (default . t)))

 ;; Showing context
 '(org-show-hierarchy-above t)
 '(org-show-following-heading t)
 '(org-show-siblings t)
 '(org-show-entry-below t)

 ;; Following Links
 '(org-file-apps (quote ((auto-mode       . emacs)
                         ("\\.mm\\'"      . default)
                         ("\\.x?html?\\'" . default)
                         ("\\.mp4\\'"     . "vlc %s"))))

 '(org-link-file-path-type 'relative)
 '(org-link-frame-setup
   (quote ((file . find-file))))

 ;; Tags:
 '(org-tag-persistent-alist
   (quote ((:startgroup  . nil)
           ("@computer"  . ?c)
           ("@desk"      . ?d)
           ("@email"     . ?e)
           ("@errand"    . ?E)
           ("@home"      . ?h)
           ("@reading"   . ?r)
           ("@phone"     . ?p)
           (:endgroup    . nil)
           (:startgroup  . nil)
           ("@online"    . ?o)
           ("@offline"   . ?O)
           (:endgroup    . nil)
           (:startgroup  . nil)
           ("5m"         . ?5)
           ("30m"        . ?3)
           ("1h"         . ?1)
           ("4h"         . ?4)
           (:endgroup    . nil))))

 ;; TODO keywords and faces:
 '(org-todo-keywords
   (quote ((sequence "TODO(t)" "|" "DONE(d)")
           (sequence "NEXT(n)" "WAITING(w)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)"))))

 '(org-todo-keyword-faces
   (quote (("TODO"    . (:foreground "#66cccc" :weight bold))
           ("NEXT"    . (:foreground "#66cccc" :weight bold))
           ("WAITING" . (:foreground "#cc99cc" :weight bold))
           ("BLOCKED" . (:inherit org-agenda-dimmed-todo-face)))))

 ;; Stuff for org-agenda.
 '(org-agenda-files
   (mapcar
    (apply-partially #'concat pjones:org-notes-directory)
    '("gtd/inbox.org"
      "gtd/projects.org"
      "gtd/rfa.org"
      "gtd/routines.org"
      "gtd/school.org")))

 '(org-agenda-window-setup (quote current-window))
 '(org-agenda-todo-ignore-with-date nil)
 '(org-agenda-todo-ignore-timestamp nil)
 '(org-agenda-todo-ignore-deadlines (quote near))
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-time-leading-zero t)
 '(org-agenda-show-inherited-tags nil)
 '(org-deadline-warning-days 14)
 '(org-agenda-span 'day)
 '(org-agenda-use-time-grid nil)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-day nil)
 '(org-agenda-block-separator ?─)

 '(org-stuck-projects
   (quote ("+project+LEVEL=3"
           ("NEXT" "WAITING" "BLOCKED") nil "")))

 `(org-agenda-custom-commands
   (quote (("c" "Current Status"
            ((agenda ""
              ((org-agenda-overriding-header "⚡ Agenda:")
               (org-agenda-remove-tags t)
               (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
               (org-agenda-prefix-format "  %-12s %-12t %-8c ")
               (org-agenda-todo-keyword-format "")))
             (todo "WAITING"
               ((org-agenda-overriding-header "⚡ Waiting for Someone Else:")
                (org-agenda-remove-tags t)
                (org-agenda-prefix-format "  %-8c ")
                (org-agenda-todo-keyword-format "")))
             (tags-todo "@phone|@email"
               ((org-agenda-overriding-header "⚡ Phone Calls to Make, Emails to Send:")
                (org-agenda-prefix-format "  %-8c ")
                (org-agenda-remove-tags t)
                (org-agenda-todo-keyword-format "")))
             (stuck ""
               ((org-agenda-overriding-header "⚡ Stuck Projects:")))
             (tags "+inbox+LEVEL=1"
               ((org-agenda-overriding-header "⚡ Inbox Tasks to Process:")
                (org-agenda-prefix-format "  %-8c ")
                (org-agenda-todo-keyword-format "")))
             (todo "NEXT"
               ((org-agenda-overriding-header "⚡ Next Actions:")
                (org-agenda-prefix-format "  %-8c ")
                (org-agenda-remove-tags nil)
                (org-agenda-todo-keyword-format "")
                (org-agenda-cmp-user-defined #'pjones:org-sort-next-actions)
                (org-agenda-sorting-strategy '(user-defined-up)))))
            nil (,(concat pjones:org-publish-directory "gtd/agenda.html")))
           ("p" "Project List"
            ((tags "+project+LEVEL=3")))
           ("T" "Travel Schedule"
            ((tags "+travel+TIMESTAMP>=\"<now>\""))
            ((org-agenda-view-columns-initially t))))))

 ;; Stuff for org-capture and org-refile:
 '(org-default-notes-file (concat pjones:org-notes-directory "gtd/inbox.org"))
 '(org-refile-use-outline-path t)
 '(org-refile-allow-creating-parent-nodes t)
 '(org-log-refile (quote time))

 ;; Preview control (more below):
 '(org-preview-latex-default-process 'imagemagick)

 ;; Stuff for exporting:
 '(org-export-with-smart-quotes t)
 '(org-icalendar-include-todo t)
 '(org-html-htmlize-output-type 'css)
 '(org-html-validation-link nil)
 '(org-highlight-latex-and-related '(native))
 '(org-latex-tables-booktabs t)
 '(org-latex-listings 'minted)
 '(org-latex-compiler "xelatex")
 '(org-format-latex-options
   '(:foreground default
     :background default
     :scale 1.5
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
      :components ("wiki" "gtd" "attachments")))))

;; Custom LaTeX classes:
(setq org-latex-classes
      (cl-remove-if
       (lambda (entry) (string-match-p "^pjones-" (car entry)))
       org-latex-classes))

;; Correctly generate LaTeX previews:
(plist-put
 (cdr (assq 'imagemagick org-preview-latex-process-alist))
 :latex-compiler '("xelatex -interaction nonstopmode -output-directory %o %f"))

(custom-set-faces
 '(org-block ((t (:background nil))))
 '(org-block-begin-line ((t (:background nil)))))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (mermaid . t)))

(push '(org-element-cache) warning-suppress-types)

(defun pjones:org-mode-hook ()
  "Hook to hack `org-mode'."
  ;; Puni doesn't work here:
  (puni-mode -1)

  ;; Buffer Settings
  (save-place-mode -1)

  ;; Tailor whitespace mode
  (setq-local whitespace-style '(trailing tabs empty))
  (whitespace-mode)

  ;; Use yasnippets:
  (yas-minor-mode)
  (setq-local yas/trigger-key [tab])
  (define-key yas/keymap [tab] #'yas-next-field)
  (add-to-list 'org-tab-first-hook #'yas-expand))

(add-hook 'org-mode-hook #'pjones:org-mode-hook)

(defun pjones:org-agenda-mode-hook ()
  "Hook run after a `org-agenda-mode' buffer is created."
  (hl-line-mode 1))

(add-hook 'org-agenda-mode-hook #'pjones:org-agenda-mode-hook)

(defun pjones:org-hide-others ()
  "Close all headings except the heading at point."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-overview)
    (org-fold-show-set-visibility 'tree)
    (org-fold-show-entry)))

(defun pjones:org-hide-all ()
  "Close all headings, move to bob."
  (interactive)
  (goto-char (point-min))
  (org-cycle '(4)))

(defun pjones:org-goto (&optional alternative-interface)
  "Call `org-goto' after first widening the buffer.
Jumps to the correct heading via `org-goto', then narrows the buffer
again if necessary.  Passes ALTERNATIVE-INTERFACE to `org-goto'."
  (interactive "P")
  (let ((point-size (save-restriction (- (point-max) (point-min))))
        (buff-size  (buffer-size)))
    (widen)
    (org-goto alternative-interface)
    (if (/= buff-size point-size) (org-narrow-to-subtree))))

(defun pjones:org-clock-update-dbus ()
  "Broadcast a D-Bus signal with the latest `org-clock' data.

This exposes the current clock's start time and heading to any process
listening to the correct D-Bus signal.

You can monitor this signal via the following command:

    \"dbus-monitor type='signal',interface='org.gnu.Emacs.Org.Clock'\"

Read the code below for the two event names and the signal arguments
they provide."
  (require 'dbus)
  (require 'org-clock)
  (if (org-clocking-p)
      (let ((start-time (floor (float-time org-clock-start-time)))
            (description org-clock-heading))
        (dbus-send-signal
         :session nil dbus-path-emacs
         (concat dbus-interface-emacs ".Org.Clock") "Started"
         start-time description))
    (dbus-send-signal
     :session nil dbus-path-emacs
     (concat dbus-interface-emacs ".Org.Clock") "Stopped")))

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

(defun pjones:org-insert-heading (&optional here)
  "Insert a heading sanely.
When HERE is non-nil, create a heading after point."
  (interactive "P")
  (let ((org-insert-heading-respect-content
         (not (or here (and (org-at-heading-p) (bolp))))))
    (if (or here (org-at-heading-p)) (org-insert-heading)
      (org-back-to-heading)
      (end-of-line)
      (org-insert-heading)))
  (when (org--blank-before-heading-p)
    (pjones:ensure-blank-lines)))

(defun pjones:org-insert-item (checkbox)
  "Insert a new item.
If CHECKBOX is non-nil, add a checkbox too.  If called from a list
item that already has a checkbox, then CHECKBOX means the opposite.

This replaces `org-insert-item' which doesn't work unless there's an
existing item.  This version works on headings too."
  (interactive "P")
  (when (org-at-item-checkbox-p)
    (setq checkbox (not checkbox)))
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
  "Arhive the current subtree to the roam daily file."
  (interactive)
  (require 'org-roam)
  (when-let* ((today (save-excursion
                       (org-roam-dailies-goto-date nil "d")
                       (buffer-file-name)))
              (org-archive-location
               (concat today "::* Archived From %s")))
    (org-archive-subtree 0)))

(defun pjones:org-attach (file)
  "Attach a FILE then insert link to it."
  (interactive "f")
  (org-attach-attach file)
  (if org-attach-store-link-p
      (org-insert-last-stored-link 1)))

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

;;; Key Bindings:
(let ((map org-mode-map))
  ;; Reset these so I can use them as a prefix:
  (define-key map (kbd "C-c C-a") nil)
  (define-key map (kbd "C-c C-e") nil)

  (define-key map (kbd "<f12>") #'org-tree-slide-mode)
  (define-key map (kbd "C-'") nil)
  (define-key map (kbd "C-<return>") #'pjones:org-insert-heading)
  (define-key map (kbd "C-c 0") #'pjones:org-hide-all)
  (define-key map (kbd "C-c 1") #'pjones:org-hide-others)
  (define-key map (kbd "C-c C-a a") #'pjones:org-attach)
  (define-key map (kbd "C-c C-a d") #'org-attach-reveal-in-emacs)
  (define-key map (kbd "C-c C-a u") #'org-attach-url)
  (define-key map (kbd "C-c C-e b") #'org-beamer-export-to-pdf)
  (define-key map (kbd "C-c C-e e") #'org-export-dispatch)
  (define-key map (kbd "C-c C-e m") #'org-gfm-export-as-markdown)
  (define-key map (kbd "C-c C-e p") #'org-latex-export-to-pdf)
  (define-key map (kbd "C-M-n") #'org-next-visible-heading)
  (define-key map (kbd "C-M-p") #'pjones:org-up-or-prev)
  (define-key map (kbd "C-o") #'pjones:org-open-line)
  (define-key map (kbd "M-<left>") #'pjones:org-promote)
  (define-key map (kbd "M-<return>") #'pjones:org-insert-item)
  (define-key map (kbd "M-<right>") #'pjones:org-demote)
  (define-key map (kbd "M-g i") #'consult-org-heading)
  (define-key map (kbd "M-n") #'org-forward-heading-same-level)
  (define-key map (kbd "M-N") #'org-metadown)
  (define-key map (kbd "M-p") #'pjones:org-backward-heading-same-level)
  (define-key map (kbd "M-P") #'org-metaup))

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

;;; Hooks
(add-hook 'org-agenda-after-show-hook #'pjones:org-hide-others)
(add-hook 'org-mode-hook #'org-appear-mode)
(add-hook 'org-mode-hook #'org-num-mode)

(let ((hooks
       '(org-clock-in-hook
         org-clock-out-hook
         org-clock-cancel-hook)))
  (dolist (hook hooks)
    (add-hook hook #'pjones:org-clock-update-dbus)))

;;; org-conf.el ends here

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
