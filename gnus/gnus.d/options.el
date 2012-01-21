(eval-when-compile
  (require 'gnus)
  (require 'nnheader)
  (require 'cl))

;; Personal Settings
(setq gnus-ignored-from-addresses
      (regexp-opt
       `(,user-mail-address "mlists@pmade.com" "suv8@pmade.org")))

;; Where things are stored
(setq gnus-startup-file      "~/.gnus.d/newsrc"
      gnus-directory         "~/.gnus.d/news"
      gnus-agent-directory   "~/.gnus.d/agent"
      message-directory      "~/.gnus.d/Mail"
      mail-default-directory "~/.gnus.d/")

;; Various Settings
(setq message-kill-buffer-on-exit t
      gnus-large-newsgroup 1000
      gnus-topic-display-empty-topics nil
      gnus-treat-date-local 'head
      gnus-read-active-file 'some)

;; MIME
(setq gnus-inhibit-mime-unbuttonizing t
      mm-automatic-display '("text/plain" "text/html")
      mm-discouraged-alternatives '("text/html"))

;; Apple Address Book
(require 'external-abook)
(setq external-abook-command "ssh renfield \"contacts -lf \'%%e\t%%n\' %s\"")

;; Don't keep back-up versions of newsrc
(defun pmade:gnus-turn-off-backup ()
  (set (make-local-variable 'backup-inhibited) t))
(add-hook 'gnus-save-quick-newsrc-hook 'pmade:gnus-turn-off-backup)
(add-hook 'gnus-save-standard-newsrc-hook 'pmade:gnus-turn-off-backup)

;; Checking for New Mail
(defun pmade-new-level-one-mail () (gnus-group-get-new-news 1))
(gnus-demon-add-handler 'pmade-new-level-one-mail 10 t)

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
(gnus-add-configuration '(group (vertical 1.0 (group 1.0 point) (calendar 0.33))))
;; (gnus-add-configuration '(summary (horizontal 1.0 (group 0.33) (summary 1.0 point))))
;; (gnus-add-configuration '(article (horizontal 1.0 (summary 0.5 point) (article 1.0))))
;; (gnus-add-configuration '(reply-yank (horizontal 1.0 (summary 0.5) (reply 1.0 point))))
;; (gnus-add-configuration '(message (horizontal 1.0 (group 0.33) (message 1.0 point) (calendar 0.33))))

(defun pmade:gnus-calendar-buffer ()
  (save-window-excursion (calendar))
  "*Calendar*")

(setq gnus-window-to-buffer (assq-delete-all 'calendar gnus-window-to-buffer))
(push (cons 'calendar 'pmade:gnus-calendar-buffer) gnus-window-to-buffer)
