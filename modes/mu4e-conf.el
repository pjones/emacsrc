;;; mu4e-conf.el -- Settings for mu4e.
(eval-when-compile
  (require 'smtpmail)
  (require 'mu4e))

;; General Settings:
(custom-set-variables
  '(message-send-mail-function 'smtpmail-send-it)
  '(message-kill-buffer-on-exit t)
  '(smtpmail-stream-type 'ssl)

  '(mu4e-maildir "~/mail")
  '(mu4e-mu-home "~/.cache/mu")
  '(mu4e-change-filenames-when-moving t)
  '(mu4e-context-policy 'ask)
  '(mu4e-compose-context-policy 'ask-if-none)
  '(mu4e-use-fancy-chars t)
  '(mu4e-headers-sort-field :date)
  '(mu4e-headers-sort-direction 'descending)
  '(mu4e-headers-results-limit -1)
  '(mu4e-date-format-long "%c")
  '(mu4e-view-show-images nil) ;; Disable tracking images!
  '(mu4e-view-show-addresses t)
  '(mu4e-view-date-format "%c")
  '(mu4e-view-scroll-to-next nil)
  '(mu4e-attachment-dir "~/download")

  '(mu4e-headers-fields
    (quote ((:human-date   . 12)
            (:flags        . 6)
            (:maildir      . 15)
            (:from         . 22)
            (:thread-subject))))

  '(mu4e-bookmarks
    (quote (("maildir:/devalot/Inbox"
             "Devalot Inbox" ?i)
            ("maildir:/rfa/Inbox"
             "RFA Inbox" ?I)
            ("flag:unread AND NOT maildir:/devalot/Trash AND NOT maildir:/rfa/\"Deleted Items\""
             "Unread messages" ?u)
            ("flag:flagged AND NOT maildir:/Trash AND NOT maildir:\"Deleted Items\""
             "Flagged messages" ?f)))))

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
                 (smtpmail-smtp-user          . "pjones")))
        (make-mu4e-context
         :name "RFA"
         :match-func #'pjones:mu4e-match-func-rfa
         :vars `((user-mail-address           . "peter.jones@rfa.sc.gov")
                 (mu4e-user-mail-address-list . '("peter.jones@rfa.sc.gov"))
                 (mu4e-sent-folder            . "/rfa/Sent Items")
                 (mu4e-drafts-folder          . "/rfa/Drafts")
                 (mu4e-trash-folder           . "/rfa/Deleted Items")
                 (mu4e-refile-folder          . "/rfa/Archive")
                 (mu4e-compose-signature      . (pjones:mu4e-read-signature "scors"))
                 (smtpmail-smtp-server        . "outlook.office365.com")
                 (smtpmail-smtp-service       . 587)
                 (smtpmail-smtp-user          . "peter.jones@rfa.sc.gov")))))
