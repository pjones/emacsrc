;;; org-conf.el -- Settings for org-mode.
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-bullets)
(require 'org-capture)
(require 's)

;; Silence compiler warnings
(declare-function dbus-send-signal "dbus")
(declare-function org-bookmark-jump-unhide "org")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-clocking-p "org-clock")
(declare-function whitespace-mode "whitespace")
(defvar whitespace-style)
(defvar org-clock-start-time)
(defvar dbus-path-emacs)
(defvar dbus-interface-emacs)

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
 '(org-clock-clocked-in-display (quote both))
 '(org-clock-mode-line-total 'current)
 '(org-show-context-detail (quote ((default . tree))))
 '(org-duration-format (quote h:mm))

 ;; Behavior Settings:
 '(org-log-done 'time)
 '(org-reverse-note-order nil)
 '(org-list-empty-line-terminates-plain-lists nil)
 '(org-blank-before-new-entry (quote ((heading . nil) (plain-list-item . nil))))
 '(org-tags-column 0)
 '(org-use-fast-todo-selection t)
 '(org-use-fast-tag-selection (quote auto))
 '(org-fast-tag-selection-single-key nil)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-M-RET-may-split-line t)
 '(org-clock-into-drawer t)
 '(org-log-into-drawer t)
 '(org-tags-exclude-from-inheritance nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-outline-path-complete-in-steps t)
 '(org-id-link-to-org-use-id t)
 '(org-edit-src-persistent-message nil)
 '(org-src-window-setup (quote current-window))
 '(org-attach-directory "attachments/")
 '(org-attach-commit nil)
 '(org-attach-git-annex-cutoff nil)
 '(org-attach-annex-auto-get nil)
 '(org-attach-method 'ln)
 '(org-attach-store-link-p t)
 '(org-attach-archive-delete nil)

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
 '(org-archive-location "::* Archived")

 '(org-refile-targets
   (quote (("~/notes/gtd/projects.org" :level . 3)
           ("~/notes/gtd/routines.org" :level . 2)
           ("~/notes/gtd/someday.org" :level . 3))))

 ;; Stuff for exporting:
 '(org-icalendar-combined-agenda-file "~/notes/gtd/calendar.ics")
 '(org-icalendar-include-todo t))

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

    dbus-monitor type='signal',interface='org.gnu.Emacs.Org.Clock'

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

;;; notmuch integration:
(defvar notmuch-show-thread-id)
(declare-function notmuch-search "notmuch")
(declare-function notmuch-search-find-thread-id "notmuch")
(declare-function notmuch-show-get-message-id "notmuch")

(defun pjones:org-notmuch-follow (id)
  "Go to mail message ID in `notmuch'."
  (require 'notmuch)
  (notmuch-search (concat "id:" id)))

(defun pjones:org-notmuch-export (link description format)
  "Export a notmuch link.
See `org-link-parameters' for details about LINK, DESCRIPTION and
FORMAT."
  ;; FIXME: For now, don't export anything.
  ;; Maybe get the subject of the email and export that?
  nil)

(defun pjones:org-notmuch-store ()
  "Store the current notmuch message as a link."
  (require 'notmuch)
  (when-let* ((query
               (cond
                ((eq major-mode 'notmuch-search-mode)
                 (notmuch-search-find-thread-id))
                ((eq major-mode 'notmuch-show-mode)
                 (notmuch-show-get-message-id))
                ((eq major-mode 'notmuch-tree-mode)
                 notmuch-show-thread-id)))
              (link (s-replace-regexp "^id:" "notmuch:" query)))
    (org-link-store-props
     :type "notmuch"
     :link link
     :description (concat "notmuch message ID `" link "'"))
    (kill-new link)))

(org-link-set-parameters
 "notmuch"
 :follow #'pjones:org-notmuch-follow
 :export #'pjones:org-notmuch-export
 :store #'pjones:org-notmuch-store)

;;; Key Bindings:
(let ((map org-mode-map))
  (define-key map (kbd "C-c C-0") #'pjones:org-hide-all)
  (define-key map (kbd "C-c C-1") #'pjones:org-hide-others))

;;; Hooks
(add-hook 'org-mode-hook #'org-bullets-mode)

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
