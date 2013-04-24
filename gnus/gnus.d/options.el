(eval-when-compile
  (require 'gnus)
  (require 'nnheader)
  (require 'cl))

;; Email addresses that get stripped from a Cc list.
(setq message-dont-reply-to-names
      '("pjones@pmade"
        "pjones@devalot"
        "pjones@contextualdevelopment"
        "mlists@pmade"
        "suv8@pmade"
        "peter\\.jones@ors\\.sc\\.gov"
        "pjones@aura-software\\.com"
        "peter@thrivesmart\\.com"))

(setq gnus-ignored-from-addresses message-dont-reply-to-names)

;; Various Settings
(setq message-kill-buffer-on-exit t
      gnus-large-newsgroup 1000
      gnus-topic-display-empty-topics nil
      gnus-treat-date-local 'head
      gnus-read-active-file 'some
      gnus-gcc-mark-as-read t)

;; MIME
(setq gnus-inhibit-mime-unbuttonizing t
      mm-automatic-display '("text/plain" "text/html")
      mm-discouraged-alternatives '("text/html")
      mm-file-name-rewrite-functions
        '(mm-file-name-trim-whitespace
          mm-file-name-collapse-whitespace
          mm-file-name-replace-whitespace))

;; Variables used in all the other files
(setq mail-signature-dir "~/develop/pmade/privaterc/signatures")

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
  (let* ((base-fmt "%m/%d/%y")
         (date (cond (header (gnus-date-get-time (mail-header-date header)))
                     (t      (gnus-group-timestamp gnus-tmp-group))))
         (since (if date (time-to-number-of-days (time-since date)) 0.0))
         (fmt (cond ((> since 1.0) (concat base-fmt " %a  "))
                    (t             (concat base-fmt " %R")))))
    (if date (format-time-string fmt date) "")))

;; Headers
(setq gnus-visible-headers
      '("^From:" "^Subject:" "^To:\\|^[BGF]?Cc:" "^Date:" "^X-Mailer:" "^X-URL:"
        "^Newsgroups:" "^Posted-To:" "^Gnus-Warning:"))

;; Window Layout
(gnus-add-configuration '(group (vertical 1.0 (group 1.0 point) (calendar 0.2))))
;; (gnus-add-configuration '(summary (horizontal 1.0 (group 0.33) (summary 1.0 point))))
;; (gnus-add-configuration '(article (horizontal 1.0 (summary 0.5 point) (article 1.0))))
;; (gnus-add-configuration '(reply-yank (horizontal 1.0 (summary 0.5) (reply 1.0 point))))
;; (gnus-add-configuration '(message (horizontal 1.0 (group 0.33) (message 1.0 point) (calendar 0.33))))

(defun pmade:gnus-calendar-buffer ()
  (save-window-excursion (calendar))
  "*Calendar*")

(setq gnus-window-to-buffer (assq-delete-all 'calendar gnus-window-to-buffer))
(push (cons 'calendar 'pmade:gnus-calendar-buffer) gnus-window-to-buffer)
