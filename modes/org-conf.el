;;; org-conf.el -- Settings for org-mode.
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'ox-md)
(require 's)

;; Silence compiler warnings
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
(declare-function org-superstar-mode "org-superstar")
(declare-function org-tree-slide-mode "org-tree-slide")
(declare-function pjones:open-line-above "../list/interactive")
(declare-function whitespace-mode "whitespace")

(defvar dbus-interface-emacs)
(defvar dbus-path-emacs)
(defvar org-attach-store-link-p)
(defvar org-clock-start-time)
(defvar whitespace-style)

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

;; General Org Settings
(custom-set-variables
 ;; Visual Settings:
 '(org-hide-leading-stars t)
 '(org-ellipsis "…")
 '(org-superstar-headline-bullets-list '(?⁍ ?◆ ?⭘ ?▶ ?◦))
 '(org-superstar-item-bullet-alist '((?* . ?○) (?+ . ?‣) (?- . ?•)))
 '(org-clock-clocked-in-display (quote both))
 '(org-clock-clocked-in-display 'frame-title)
 '(org-clock-frame-title-format (list '(t org-mode-line-string) " " frame-title-format))
 '(org-clock-mode-line-total 'current)
 '(org-show-context-detail (quote ((default . tree))))
 '(org-duration-format (quote h:mm))
 '(org-hide-emphasis-markers t)
 '(org-adapt-indentation t)
 '(org-appear-autolinks t)
 '(org-appear-autosubmarkers t)
 '(org-appear-autoentities t)
 '(org-appear-autokeywords t)

 ;; Behavior Settings:
 '(org-catch-invisible-edits 'smart)
 '(org-log-done 'time)
 '(org-reverse-note-order nil)
 '(org-list-empty-line-terminates-plain-lists nil)
 '(org-tags-column 0)
 '(org-use-fast-todo-selection 'expert)
 '(org-use-fast-tag-selection (quote auto))
 '(org-fast-tag-selection-single-key nil)
 '(org-imenu-depth 3)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-M-RET-may-split-line t)
 '(org-clock-into-drawer t)
 '(org-log-into-drawer t)
 '(org-tags-exclude-from-inheritance nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-outline-path-complete-in-steps nil)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-edit-src-persistent-message nil)
 '(org-src-window-setup (quote current-window))
 '(org-attach-id-dir "~/notes/attachments/")
 '(org-attach-auto-tag nil)
 '(org-attach-dir-relative t)
 '(org-attach-method 'ln)
 '(org-attach-store-link-p 'attached)
 '(org-attach-archive-delete nil)
 '(org-capture-bookmark nil)
 '(org-archive-file-header-format nil)

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
   (quote ("~/notes/gtd/routines.org"
           "~/notes/gtd/projects.org"
           "~/notes/gtd/inbox.org")))

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
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-day nil)
 '(org-agenda-block-separator ?─)

 '(org-stuck-projects
   (quote ("+project+LEVEL=3"
           ("NEXT" "WAITING" "BLOCKED") nil "")))

 '(org-agenda-custom-commands
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
                (org-agenda-sorting-strategy '(user-defined-up))))))
           ("p" "Project List"
            ((tags "+project+LEVEL=3")))
           ("T" "Travel Schedule"
            ((tags "+travel+TIMESTAMP>=\"<now>\""))
            ((org-agenda-view-columns-initially t))))))

 ;; Stuff for org-capture and org-refile:
 '(org-default-notes-file "~/notes/gtd/inbox.org")
 '(org-refile-use-outline-path t)
 '(org-refile-allow-creating-parent-nodes t)
 '(org-log-refile (quote time))

 '(org-refile-targets
   (quote (("~/notes/gtd/projects.org" :level . 3)
           ("~/notes/gtd/routines.org" :level . 2)
           ("~/notes/gtd/someday.org" :level . 3))))

 ;; Stuff for exporting:
 '(org-export-with-smart-quotes t)
 '(org-icalendar-combined-agenda-file "~/notes/gtd/calendar.ics")
 '(org-icalendar-include-todo t)
 '(org-html-htmlize-output-type 'css)
 '(org-html-validation-link nil)
 '(org-latex-tables-booktabs t)
 '(org-latex-listings 'minted)
 '(org-latex-default-class "pjones-article")
 '(org-latex-compiler "xelatex")
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

 '(org-publish-project-alist
   '(("gtd"
      :base-directory "~/notes/gtd/"
      :base-extension "org"
      :recursive t
      :exclude "archive\\.org$"
      :publishing-function org-html-publish-to-html
      :publishing-directory "~/public/gtd/"
      :section-numbers t
      :with-broken-links t
      :with-toc 2
      :archived-trees nil
      :html-link-home "../wiki/index.html"
      :html-link-up "../wiki/sitemap.html"
      :html-home/up-format
      "<div id=\"org-div-home-and-up\">
      <a title=\"Topics\" href=\"%s\">🌎</a>
      <a title=\"Home\" href=\"%s\">🏠</a>
      </div>")
     ("wiki"
      :base-directory "~/notes/wiki/"
      :base-extension "org"
      :recursive t
      :auto-sitemap t
      :sitemap-title "Peter's Knowledge Base (All Pages)"
      :sitemap-filename "sitemap.org"
      :sitemap-function pjones:org-publish-sitemap
      :sitemap-sort-folders ignore
      :sitemap-style list ; Tree is broken :(
      :preparation-function pjones:org-roam-before-publish
      :completion-function pjones:org-roam-after-publish
      :publishing-function org-html-publish-to-html
      :publishing-directory "~/public/wiki"
      :section-numbers t
      :with-broken-links t
      :with-toc nil
      :html-link-home "../../../index.html"
      :html-link-up "../../../sitemap.html"
      :html-home/up-format
      "<div id=\"org-div-home-and-up\">
      <a title=\"Topics\" href=\"%s\">🌎</a>
      <a title=\"Home\" href=\"%s\">🏠</a>
      </div>")
     ("attachments"
      :base-directory "~/notes/attachments/"
      :base-extension 'any
      :recursive t
      :publishing-directory "~/public/attachments/"
      :publishing-function org-publish-attachment)
     ("notes"
      :components ("wiki" "gtd" "attachments")))))

;; Custom LaTeX classes:
(setq org-latex-classes
      (cl-remove-if
       (lambda (entry) (string-match-p "^pjones-" (car entry)))
       org-latex-classes))

(add-to-list
 'org-latex-classes
 (append `("pjones-article"
           ,(string-join '("\\documentclass[11pt]{article}"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "\\setmainfont{Noto Serif Light}"
                           "\\setsansfont{Noto Sans}"
                           "\\setmonofont[Scale=0.85]{Hermit}")
                         "\n"))
         (cddr (assoc "article" org-latex-classes))))

(custom-set-faces
 '(org-block ((t (:background nil))))
 '(org-block-begin-line ((t (:background nil))))
 '(org-block-end-line ((t (:background nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.7))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0)))))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (mermaid . t)))


(defun pjones:org-mode-hook ()
  "Hook to hack `org-mode'."
  ;; Buffer Settings
  (save-place-mode -1)

  ;; Tailor whitespace mode
  (setq-local whitespace-style '(trailing tabs empty))
  (whitespace-mode))

(add-hook 'org-mode-hook #'pjones:org-mode-hook)

(defun pjones:org-agenda-mode-hook ()
  "Hook run after a `org-agenda-mode' buffer is created."
  (hl-line-mode 1))

(add-hook 'org-agenda-mode-hook #'pjones:org-agenda-mode-hook)

(defun pjones:org-hide-others ()
  "Close all headings except the heading at point."
  (interactive)
  (org-back-to-heading)
  (org-overview)
  (org-show-set-visibility 'tree)
  (org-show-entry))

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
          (org-show-context)
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

(defun pjones:org-insert-heading ()
  "Insert a heading sanely."
  (interactive)
  (if (bolp) (org-insert-heading)
    (org-insert-heading-after-current)))

(defun pjones:org-insert-item (checkbox)
  "Insert a new item.
If CHECKBOX is non-nil, add a checkbox too.
This replaces `org-insert-item' which doesn't work unless there's an
existing item.  This version works on headings too."
  (interactive "P")
  (unless (org-insert-item checkbox)
    (org-back-to-heading)
    (org-show-subtree)
    (let ((here (point)))
      (outline-next-heading)
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
      (when checkbox (insert "[ ] ")))))

(defun pjones:org-open-line (arg)
  "Open a line, the correct way.
If ARG is non-nil then open below instead of above.  Like the original
version, properly handles tables."
  (interactive "P")
  (if (org-at-table-p) (org-table-insert-row arg)
    (pjones:open-line-above arg)))

(defun pjones:org-archive-subtree-to-daily ()
  "Arhive the current subtree to the roam daily file."
  (interactive)
  (require 'org-roam)
  (when-let* ((today (save-excursion
                       (org-roam-dailies-goto-date)
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

;;; Key Bindings:
(let ((map org-mode-map))
  ;; Reset this so I can use it as a prefix:
  (define-key map (kbd "C-c C-a") nil)

  (define-key map (kbd "<f12>") #'org-tree-slide-mode)
  (define-key map (kbd "C-'") nil)
  (define-key map (kbd "C-<return>") #'pjones:org-insert-heading)
  (define-key map (kbd "C-c 0") #'pjones:org-hide-all)
  (define-key map (kbd "C-c 1") #'pjones:org-hide-others)
  (define-key map (kbd "C-c C-a a") #'pjones:org-attach)
  (define-key map (kbd "C-c C-a d") #'org-attach-reveal-in-emacs)
  (define-key map (kbd "C-c C-a u") #'org-attach-url)
  (define-key map (kbd "C-c C-x a") #'pjones:org-archive-subtree-to-daily)
  (define-key map (kbd "C-c C-x A") #'pjones:org-archive-subtree-to-daily)
  (define-key map (kbd "C-M-n") #'org-next-visible-heading)
  (define-key map (kbd "C-M-p") #'pjones:org-up-or-prev)
  (define-key map (kbd "C-o") #'pjones:org-open-line)
  (define-key map (kbd "M-<return>") #'pjones:org-insert-item)
  (define-key map (kbd "M-n") #'org-forward-heading-same-level)
  (define-key map (kbd "M-N") #'org-metadown)
  (define-key map (kbd "M-p") #'org-backward-heading-same-level)
  (define-key map (kbd "M-P") #'org-metaup))

;;; Hooks
(add-hook 'org-mode-hook #'org-appear-mode)
(add-hook 'org-mode-hook #'org-superstar-mode)

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
