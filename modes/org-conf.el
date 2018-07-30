;;; org-conf.el -- Settings for org-mode.
;; Silence compiler warnings
(declare-function whitespace-mode "whitespace")
(declare-function org-bookmark-jump-unhide "org")

(require 'saveplace)
(require 'whitespace)
(require 'org)
(require 'org-clock)
(require 'org-agenda)
(require 'org-id)

;; General Org Settings
(custom-set-variables
 ;; Visual Settings:
 '(org-hide-leading-stars t)
 '(org-time-clocksum-format "%02d:%02d")
 '(org-clock-clocked-in-display (quote both))
 '(org-clock-mode-line-total 'current)
 '(org-show-context-detail (quote ((default . tree))))

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
 '(org-completion-use-ido t)
 '(org-goto-interface 'outline-path-completion)
 '(org-outline-path-complete-in-steps t)
 '(org-id-link-to-org-use-id t)
 '(org-edit-src-persistent-message nil)
 '(org-src-window-setup (quote current-window))

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
           ("@kindle"    . ?k)
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
           (sequence "|" "NOTES")
           (sequence "NEXT(n)" "WAITING(w)" "DEPENDS(s)" "|" "DONE(d)" "CANCELLED(c)"))))

 '(org-todo-keyword-faces
   (quote (("TODO"    . (:foreground "#66cccc" :weight bold))
           ("NEXT"    . (:foreground "#66cccc" :weight bold))
           ("WAITING" . (:foreground "#cc99cc" :weight bold))
           ("DEPENDS" . (:inherit org-agenda-dimmed-todo-face)))))

 ;; Stuff for org-agenda.
 '(org-agenda-files
   (quote ("~/notes/agenda/projects.org"
           "~/notes/agenda/tasks.org"
           "~/notes/agenda/review.org"
           "~/notes/agenda/calendar.org")))

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

 '(org-stuck-projects
   (quote ("+project+LEVEL=3|+tasks+LEVEL=3-inbox-TODO=\"DONE\""
           ("*") nil "")))

 '(org-agenda-custom-commands
   (quote (("c" "Current Status"
            ((agenda)
             (tags-todo "@phone"
                   ((org-agenda-todo-ignore-deadlines (quote all))
                    (org-agenda-todo-ignore-scheduled (quote all))))
             (tags-todo "@email"
                   ((org-agenda-todo-ignore-deadlines (quote all))
                    (org-agenda-todo-ignore-scheduled (quote all))))
             (todo "WAITING"
                   ((org-agenda-todo-ignore-deadlines (quote future))
                    (org-agenda-todo-ignore-scheduled (quote future))))
             (stuck)
             (tags "+inbox+LEVEL=2")))
           ("p" "Project List"
            ((tags "+project+LEVEL=3")))
           ("e" "Tasks by Energy Level"
            ((tags-todo "5m")
             (tags-todo "30m")
             (tags-todo "1h"))
            ((org-agenda-todo-ignore-deadlines nil)))
           ("o" "Offline Tasks"
            ((tags-todo "+@offline")))
           ("T" "Travel Schedule"
            ((tags "+travel+TIMESTAMP>=\"<now>\""))
            ((org-agenda-view-columns-initially t))))))

 ;; Stuff for org-capture and org-refile:
 '(org-default-notes-file "~/notes/agenda/tasks.org")
 '(org-refile-use-outline-path t)
 '(org-refile-allow-creating-parent-nodes t)
 '(org-log-refile (quote time))
 '(org-archive-location "~/notes/agenda/archive/%s::")

 '(org-refile-targets
   (quote (("~/notes/agenda/projects.org" :level . 3)
           ("~/notes/agenda/tasks.org" :level . 1)
           ("~/notes/agenda/review.org" :level . 2)
           ("~/notes/agenda/calendar.org" :level . 1))))

 '(org-capture-templates
   (quote (("i" "Inbox" entry (id "9a053ee1-ade2-43af-af63-ea3e28fcc639")
            (file "~/notes/etc/templates/orgmode/inbox.org"))
           ("t" "Standalone Task" entry (id "f058299a-ab32-4374-9d38-2ef744c3f306")
            (file "~/notes/etc/templates/orgmode/task.org"))
           ("o" "Prep for Upcoming Offline" entry (id "6224680c-d1c7-4e3a-825e-affa4a065345")
            (file "~/notes/etc/templates/orgmode/task.org"))
           ("r" "Respond to Email" entry (id "3ef415be-890a-407b-8a3e-21814810790e")
            (file "~/notes/etc/templates/orgmode/mail.org"))
           ("c" "New Training Class" entry (id "09727f4a-aa01-4429-8408-d40511c19657")
            (file "~/notes/etc/templates/orgmode/training.org"))))))

(defun pjones:org-mode-hook ()
  ;; Extra Bindings
  (org-defkey org-mode-map "\C-\M-f"   'org-metaright)
  (org-defkey org-mode-map "\C-\M-b"   'org-metaleft)
  (org-defkey org-mode-map "\C-\M-S-f" 'org-shiftmetaright)
  (org-defkey org-mode-map "\C-\M-S-b" 'org-shiftmetaleft)
  (org-defkey org-mode-map "\C-\M-p"   'org-metaup)
  (org-defkey org-mode-map "\C-\M-n"   'org-metadown)

  (org-defkey org-mode-map "\C-ce"               'pjones:org-edit-special)
  (org-defkey org-mode-map "\C-c0"               'pjones:org-hide-all)
  (org-defkey org-mode-map "\C-c1"               'pjones:org-hide-others)
  (org-defkey org-mode-map "\C-j"                'pjones:org-list-append)
  (org-defkey org-mode-map "\C-c\C-j"            'pjones:org-goto)
  (org-defkey org-mode-map [(meta return)]       'pjones:org-list-append)
  (org-defkey org-mode-map [(shift meta return)] 'pjones:org-list-append-with-checkbox)

  ;; Buffer Settings
  (save-place-mode -1)

  ;; Exporting
  ;;(setq org-export-html-link-org-files-as-html nil
  ;;      org-export-with-emphasize nil
  ;;      org-export-html-style-default ""
  ;;      org-export-html-style-extra ""
  ;;      org-export-html-style nil)

  ;; Tailor whitespace mode
  (set (make-local-variable 'whitespace-style)
       '(trailing tabs empty))
  (whitespace-mode))

(add-hook 'org-mode-hook 'pjones:org-mode-hook)

(defun pjones:org-agenda-mode-hook ()
  "Hook run after a `org-agenda-mode' buffer is created."
  (hl-line-mode 1))

(add-hook 'org-agenda-mode-hook 'pjones:org-agenda-mode-hook)

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

(defun pjones:org-list-append (&optional checkbox)
  "Append a plain list item to the current heading.  If the
current heading already has plain list items, a new one will be
added, otherwise a new plain list will be created.  If checkbox
is set, add a plain list item with a checkbox."
  (interactive "P")
  (when (not (org-insert-item (if checkbox 'checkbox)))
    (org-back-to-heading)
    (org-show-subtree)
    (outline-next-heading)
    (if (eolp) (newline)
      (newline)
      (forward-line -1))
    (org-indent-line)
    (insert (concat "-" (if checkbox " [ ] " " ")))
    (save-excursion
      (org-back-to-heading)
      (org-cycle-hide-drawers 'subtree))))

(defun pjones:org-list-append-with-checkbox ()
  "Calls `pjones:org-list-append' with checkbox set."
  (interactive)
  (pjones:org-list-append t))

(defun pjones:org-time-diff (t1 t2)
  "Returns the difference between t1 and t2.  Expects that times
are formatted as HH:MM and returns them in that format"
  (org-minutes-to-clocksum-string
   (- (org-hh:mm-string-to-minutes t1)
      (org-hh:mm-string-to-minutes t2))))

(defun pjones:org-goto (&optional alternative-interface)
  "My version of `org-goto' that first widens the buffer (if
narrowed), jumps to the correct heading via `org-goto', then
narrows it again if necessary."
  (interactive "P")
  (let ((point-size (save-restriction (- (point-max) (point-min))))
        (buff-size  (buffer-size)))
    (widen)
    (org-goto alternative-interface)
    (if (/= buff-size point-size) (org-narrow-to-subtree))))

(defun pjones:org-edit-special (&optional arg)
  "A wrapper around `org-edit-special' that acts differently from
within a terminal vs. a graphical client.  I typically do
presentations from within a terminal and tmux session and in that
case I don't want org to mess around with the window
arrangement."
  (interactive "P")
  (let ((org-src-window-setup
         (if (display-graphic-p) 'reorganize-frame
           'current-window)))
    (org-edit-special arg)))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
