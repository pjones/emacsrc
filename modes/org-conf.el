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

 ;; Behavior Settings:
 '(org-log-done (quote note))
 '(org-reverse-note-order nil)
 '(org-deadline-warning-days 14)
 '(org-list-empty-line-terminates-plain-lists nil)
 '(org-blank-before-new-entry (quote ((heading . nil) (plain-list-item . nil))))
 '(org-use-fast-todo-selection t)
 '(org-use-fast-tag-selection (quote auto))
 '(org-fast-tag-selection-single-key t)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-M-RET-may-split-line t)
 '(org-time-clocksum-format "%02d:%02d")
 '(org-clock-into-drawer t)
 '(org-clock-clocked-in-display (quote both))
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
 '(org-link-frame-setup (quote ((file . find-file))))
 '(org-file-apps (quote ((auto-mode . emacs)
                         ("\\.mm\\'" . default)
                         ("\\.x?html?\\'" . default)
                         ("\\.mp4\\'" . "vlc %s")
                         ("\\.pdf\\'" . "zathura %s"))))

 ;; Tags:
 '(org-tag-persistent-alist
   (quote (("@computer"  . ?c)
           ("@desk"      . ?d)
           ("@email"     . ?e)
           ("@home"      . ?h)
           ("@online"    . ?o)
           ("@phone"     . ?p)
           ("@plane"     . ?P)
           ("@traveling" . ?t)
           (:startgroup  . nil)
           ("5m"         . ?5)
           ("30m"        . ?3)
           ("1h"         . ?1)
           (:endgroup    . nil))))

 ;; TODO keywords and faces:
 '(org-todo-keywords
   (quote ((sequence "TODO(t)" "|" "DONE(d)")
           (sequence "NEXT(n)" "WAITING(w)" "DEPENDS(s)" "|" "DONE(d)" "CANCELLED(c)"))))

 '(org-todo-keyword-faces
   (quote (("NEXT"    . (:inherit 'mode-line :background "#268bd2"))
           ("WAITING" . (:inherit 'mode-line :background "#d33682"))
           ("DEPENDS" . (:inherit 'mode-line :background "#2aa198")))))

 ;; Stuff for org-agenda.
 '(org-agenda-files
   (quote ("~/notes/agenda/projects.org"
           "~/notes/agenda/inbox.org")))


 '(org-agenda-todo-ignore-with-date t)
 '(org-agenda-todo-ignore-timestamp t)
 '(org-agenda-todo-ignore-scheduled t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-start-with-follow-mode t)
 '(org-agenda-time-leading-zero t)
 '(org-agenda-span (quote fortnight))
 '(org-stuck-projects (quote ("+LEVEL=3/-DONE"
                              ("TODO" "NEXT" "WAITING" "DEPENDS")
                              nil "")))
 '(org-agenda-custom-commands
   (quote (("p" "Projects" ((agenda)
                            (todo "TODO|NEXT")
                            (todo "WAITING")
                            (stuck)))))))

;; (defadvice org-agenda (around pjones:agenda-remember-windows activate)
;;   (window-configuration-to-register :org-agenda-windows)
;;   ad-do-it
;;   (jump-to-register :org-agenda-windows)
;;   (set-window-buffer (selected-window) "*Org Agenda*"))

;; (defun pjones:org-agenda-quit ()
;;   "Restores the previous window configuration and kills the
;; agenda buffer."
;;   (interactive)
;;   (org-agenda-quit)
;;   (jump-to-register :org-agenda-windows))

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

;; (defun pjones:org-agenda-mode-hook ()
;;   (define-key org-agenda-mode-map (kbd "q") 'pjones:org-agenda-quit))
;; (add-hook 'org-agenda-mode-hook 'pjones:org-agenda-mode-hook)

(defun pjones:org-hide-others ()
  "Close all headings except the heading at point."
  (interactive)
  (org-overview)
  (org-reveal))

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
