;;; mu4e-conf.el -- Settings for mu4e.
(eval-when-compile
  (require 'smtpmail)
  (require 'mu4e))

;; Dependencies:
(require 'org-mu4e)
(require 'mu4e-query-fragments)

;; Functions:
(defun pjones:mu4e-match-func-devalot (msg)
  "Return non-nil if MSG is associated with the Devalot account."
  (when msg
    (string-prefix-p "/devalot" (mu4e-message-field msg :maildir))))

(defun pjones:mu4e-match-func-rfa (msg)
  "Return non-nil if MSG is associated with the RFA account."
  (when msg
    (string-prefix-p "/rfa" (mu4e-message-field msg :maildir))))

(defun pjones:mu4e-read-signature (name)
  "Return a mail signature from file NAME."
  (let ((sig-dir "~/core/privaterc/signatures/"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name (concat sig-dir name)))
      (buffer-string))))

(defun pjones:mu4e-make-queue-directory ()
  "Ensure the send queue directory exists."
  (let* ((cur (expand-file-name smtpmail-queue-dir))
         (dir (file-name-directory (directory-file-name cur))))
    (unless (file-exists-p cur)
      (call-process "mu" nil nil nil "mkdir" dir)
      (call-process "touch" nil nil nil (concat dir ".noindex")))))

(defun pjones-mu4e-short-maildir (msg)
  "Format the maildir of MSG so it's as short as possible."
  (let* ((maildir (or (mu4e-message-field msg :maildir) ""))
         (prefix (substring maildir 1 2))
         (dir (file-name-nondirectory maildir)))
    (concat prefix "/" dir)))

;; General Settings:
(custom-set-variables
  '(mail-user-agent 'mu4e-user-agent)
  '(message-send-mail-function 'smtpmail-send-it)
  '(message-kill-buffer-on-exit t)
  '(smtpmail-queue-dir "~/mail/queue/cur")

  '(mu4e-maildir "~/mail")
  '(mu4e-mu-home "~/.cache/mu")
  '(mu4e-change-filenames-when-moving t)
  '(mu4e-context-policy 'pick-first)
  '(mu4e-compose-context-policy 'ask-if-none)
  '(mu4e-use-fancy-chars t)
  '(mu4e-headers-sort-field :date)
  '(mu4e-headers-sort-direction 'ascending)
  '(mu4e-headers-results-limit -1)
  '(mu4e-date-format-long "%c")
  '(mu4e-view-show-images nil) ;; Disable tracking images!
  '(mu4e-view-show-addresses t)
  '(mu4e-view-date-format "%c")
  '(mu4e-view-scroll-to-next nil)
  '(mu4e-attachment-dir "~/download")

  '(mu4e-headers-fields
    (quote ((:human-date    . 12)
            (:flags         . 6)
            (:short-maildir . 15)
            (:from          . 22)
            (:thread-subject))))

  '(mu4e-query-fragments-list
    (quote (("%trash" . "( maildir:/devalot/Trash OR maildir:/rfa/\"Deleted Items\" )")
            ("%list"  . "maildir:/devalot/mlists/*"))))

  '(mu4e-bookmarks
    (quote (("maildir:/devalot/Inbox" "Devalot Inbox" ?i)
            ("maildir:/rfa/Inbox" "RFA Inbox" ?I)
            ("flag:unread AND NOT %trash AND NOT %list" "Unread messages" ?u)
            ("flag:unread AND %list" "Unread lists" ?l)
            ("flag:flagged AND NOT %trash" "Flagged messages" ?f)))))

;; Contexts:
(setq mu4e-contexts
  (list (make-mu4e-context
         :name "Devalot"
         :match-func #'pjones:mu4e-match-func-devalot
         :vars `((user-mail-address           . "pjones@devalot.com")
                 (mu4e-user-mail-address-list . ("pjones@devalot.com" "pjones@pmade.com"))
                 (mu4e-sent-folder            . "/devalot/Sent")
                 (mu4e-drafts-folder          . "/devalot/Drafts")
                 (mu4e-trash-folder           . "/devalot/Trash")
                 (mu4e-refile-folder          . "/devalot/Archive")
                 (mu4e-compose-signature      . ,(pjones:mu4e-read-signature "devalot"))
                 (smtpmail-smtp-server        . "mail.pmade.com")
                 (smtpmail-smtp-service       . 465)
                 (smtpmail-stream-type        . ssl)
                 (smtpmail-smtp-user          . "pjones")))
        (make-mu4e-context
         :name "RFA"
         :match-func #'pjones:mu4e-match-func-rfa
         :vars `((user-mail-address           . "peter.jones@rfa.sc.gov")
                 (mu4e-user-mail-address-list . ("peter.jones@rfa.sc.gov"))
                 (mu4e-sent-folder            . "/rfa/Sent Items")
                 (mu4e-drafts-folder          . "/rfa/Drafts")
                 (mu4e-trash-folder           . "/rfa/Deleted Items")
                 (mu4e-refile-folder          . "/rfa/Archive")
                 (mu4e-compose-signature      . ,(pjones:mu4e-read-signature "scors"))
                 (smtpmail-smtp-server        . "outlook.office365.com")
                 (smtpmail-smtp-service       . 587)
                 (smtpmail-stream-type        . starttls)
                 (smtpmail-smtp-user          . "peter.jones@rfa.sc.gov")))))

;; Extra Headers:
(add-to-list 'mu4e-header-info-custom
  '(:ua . (:name "User-Agent"
           :shortname "UA"
           :help "Mail User Agent"
           :function (lambda (msg)
                       (or (mu4e-message-field msg :user-agent) "")))))

(add-to-list 'mu4e-header-info-custom
  '(:short-maildir . (:name "Short Maildir"
                      :shortname "Dir"
                      :help "Shortened Maildir Name"
                      :function (lambda (msg)
                                  (pjones-mu4e-short-maildir msg)))))

(add-to-list 'mu4e-view-fields :ua t)

;; View Actions:
(add-to-list 'mu4e-view-actions '("open in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions '("tag message"     . mu4e-action-retag-message) t)

;; Hooks
(add-hook 'mu4e-compose-pre-hook #'pjones:mu4e-make-queue-directory)
