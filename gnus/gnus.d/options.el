(eval-when-compile
  (require 'gnus)
  (require 'nnheader)
  (require 'cl))

;; Personal Settings
(setq gnus-ignored-from-addresses
      (regexp-opt
       `(,user-mail-address "mlists@pmade.com" "suv8@pmade.org")))

;; Various Settings
(setq
 message-kill-buffer-on-exit t
 gnus-large-newsgroup 1000
 gnus-topic-display-empty-topics nil
 gnus-treat-date-local 'head
 gnus-read-active-file 'some)           ; Speed up initial load

;; File locations
(setq
 pmade-comm-base "~/.comm-sync"
 pmade-gnus-var (concat pmade-comm-base "/var/gnus")
 pmade-gnus-etc (concat pmade-comm-base "/etc/gnus")
 pmade-sigs-dir (concat pmade-comm-base "/etc/signatures")
 pmade-authinfo (concat pmade-gnus-etc "/authinfo")
 gnus-directory pmade-gnus-var
 mail-signature-dir pmade-sigs-dir
 gnus-agent-directory (concat pmade-gnus-var "/agent")
 gnus-article-save-directory (concat pmade-gnus-var "/news")
 gnus-cache-directory (concat pmade-gnus-var "/cache")
 gnus-startup-file (concat pmade-gnus-etc "/newsrc")
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil)

;; MIME
(eval-after-load "mm-decode"
  '(progn
     (dolist (i '("text/html" "text/richtext" "multipart/mixed" "multipart/related"))
       (add-to-list 'mm-discouraged-alternatives i)
       (setq mm-automatic-display (remove i mm-automatic-display)))))

;; Apple Address Book
(require 'external-abook)
(setq external-abook-command "contacts -lf '%%e\t%%n' %s")

;; Checking for New Mail
(defun pmade-new-level-one-mail () (gnus-group-get-new-news 1))
;; (gnus-demon-add-handler 'pmade-new-level-one-mail 20 t)
;; (gnus-demon-init)

;; Highlight the current line in the groups and summary buffers
(defun pmade-gnus-index-hook ()
  "Things to do in any Gnus buffer that is an index (like the
group and summary buffers)"
  (hl-line-mode 1)
  (setq cursor-type nil))

(add-hook 'gnus-group-mode-hook   'pmade-gnus-index-hook)
(add-hook 'gnus-summary-mode-hook 'pmade-gnus-index-hook)

;; User Format Functions
(defun gnus-user-format-function-d (header)
  "Display group and article dates in an easy to read format."
  (let ((date))
    (cond
     (header (setq date (gnus-date-get-time (mail-header-date header))))
     (t      (setq date (gnus-group-timestamp gnus-tmp-group))))
    (if date (format-time-string "%a %b %d, %Y %R" date) "")))

;; Headers
(setq gnus-visible-headers
      '("^From:" "^Subject:" "^To:\\|^[BGF]?Cc:" "^Date:" "^X-Mailer:" "^X-URL:"
        "^Newsgroups:" "^Posted-To:" "^Gnus-Warning:"))

;; Window Layout
(gnus-add-configuration '(group (horizontal 1.0 (calendar 0.33) (group 1.0 point) (scratch 0.33))))
(gnus-add-configuration '(summary (horizontal 1.0 (group 0.33) (summary 1.0 point) (scratch 0.33))))
(gnus-add-configuration '(article (horizontal 1.0 (summary 0.33 point) (article 1.0) (scratch 0.33))))
(gnus-add-configuration '(reply-yank (horizontal 1.0 (summary 0.33) (reply 1.0 point) (scratch 0.33))))
(gnus-add-configuration '(message (horizontal 1.0 (group 0.33) (calendar 0.33) (message 1.0 point))))

(when (string= "skinny.local" system-name)
  (gnus-add-configuration '(group (horizontal 1.0 (group 1.0 point) (scratch 0.5))))
  (gnus-add-configuration '(summary (horizontal 1.0 (group 0.5) (summary 1.0 point))))
  (gnus-add-configuration '(article (horizontal 1.0 (summary 0.5 point) (article 1.0))))
  (gnus-add-configuration '(reply-yank (horizontal 1.0 (summary 0.5) (reply 1.0 point))))
  (gnus-add-configuration '(message (horizontal 1.0 (group 0.5) (message 1.0 point)))))

;; Window Layout Helpers
(defun pmade:gnus-reply-buffer ()
  (let (replies)
    (dolist (buffer (buffer-list))
      (when (string-match "^\\*\\(wide \\)?reply" (buffer-name buffer))
        (push (buffer-name buffer) replies)))
    (car replies)))

(defun pmade:gnus-calendar-buffer ()
  (save-window-excursion (calendar))
  "*Calendar*")

(setq gnus-window-to-buffer (assq-delete-all 'reply gnus-window-to-buffer))
(push (cons 'reply 'pmade:gnus-reply-buffer) gnus-window-to-buffer)

(setq gnus-window-to-buffer (assq-delete-all 'calendar gnus-window-to-buffer))
(push (cons 'calendar 'pmade:gnus-calendar-buffer) gnus-window-to-buffer)

(setq gnus-window-to-buffer (assq-delete-all 'scratch gnus-window-to-buffer))
(push (cons 'scratch "*scratch*") gnus-window-to-buffer)
