;;; mu4e-conf.el -- Settings for mu4e.
;;
;;; Commentary:
;;
;;; Code:
(require 'smtpmail)
(require 'mu4e)
(require 'org-mu4e)

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
  (dolist (ctx mu4e-contexts)
    (let* ((vars (mu4e-context-vars ctx))
           (cur (expand-file-name (alist-get 'smtpmail-queue-dir vars)))
           (dir (file-name-directory (directory-file-name cur))))
      (unless (file-exists-p cur)
        (make-directory (file-name-directory dir) t)
        (call-process "mu" nil nil nil "mkdir" dir)
        (call-process "touch" nil nil nil (concat dir ".noindex"))))))

(defun pjones-mu4e-short-maildir (msg)
  "Format the maildir of MSG so it's as short as possible."
  (let* ((maildir (or (mu4e-message-field msg :maildir) ""))
         (prefix (substring maildir 1 2))
         (dir (file-name-nondirectory maildir)))
    (concat prefix "/" dir)))

(defvar pjones:mu4e-trash
  "(maildir:/devalot/Trash OR maildir:/rfa/\"Deleted Items\")"
  "A mu query for trash folders.")

(defvar pjones:mu4e-mlists
  "maildir:/devalot/mlists"
  "A mu query for mailing lists folder.")

;; General Settings:
(custom-set-variables
  '(mail-user-agent 'mu4e-user-agent)
  '(message-send-mail-function 'smtpmail-send-it)
  '(message-kill-buffer-on-exit t)
  '(smtpmail-queue-mail nil)
  '(smtpmail-queue-dir "~/mail/queue/devalot/cur")

  '(mu4e-maildir "~/mail")
  '(mu4e-mu-home "~/.cache/mu")
  '(mu4e-get-mail-command "mbsync-pjones")
  '(mu4e-update-interval nil)
  '(mu4e-hide-index-messages t)
  '(mu4e-change-filenames-when-moving t)
  '(mu4e-context-policy 'pick-first)
  '(mu4e-compose-context-policy 'ask-if-none)
  '(mu4e-use-fancy-chars nil)
  '(mu4e-headers-sort-field :date)
  '(mu4e-headers-sort-direction 'ascending)
  '(mu4e-headers-results-limit 200)
  '(mu4e-headers-include-related nil)
  '(mu4e-headers-show-threads nil)
  '(mu4e-date-format-long "%c")
  '(mu4e-view-show-images nil) ;; Disable tracking images!
  '(mu4e-view-show-addresses t)
  '(mu4e-view-date-format "%c")
  '(mu4e-view-scroll-to-next nil)
  '(mu4e-attachment-dir "~/download")
  '(mu4e-completing-read-function 'completing-read)

  '(mu4e-headers-fields
    (quote ((:human-date    . 12)
            (:flags         . 6)
            (:short-maildir . 15)
            (:from          . 22)
            (:thread-subject))))

  '(mu4e-maildir-shortcuts
    (quote (("/devalot/Inbox" . ?i)
            ("/devalot/Junk"  . ?j)
            ("/rfa/Inbox"     . ?I))))

  `(mu4e-bookmarks
    (quote ((,(concat "flag:unread AND NOT " pjones:mu4e-trash
                      " AND NOT " pjones:mu4e-mlists
                      " AND NOT m:/devalot/Archive")
             "Unread messages" ?u)

            (,(concat "flag:unread AND " pjones:mu4e-mlists) "Unread lists" ?l)
            ("m:/devalot/Sent d:today..now" "Sent today" ?s)
            ("m:/devalot/Archive d:1w..now" "Archived this week" ?a)
            ("flag:flagged" "Flagged messages" ?f)))))

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
                 (smtpmail-queue-dir          . "~/mail/queue/devalot/cur")
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
                 (smtpmail-queue-dir          . "~/mail/queue/rfa/cur")
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

;; Bug fixes:

(defun evil-collection-mu4e-new-region-misc ()
  "Define the evil-mu4e Misc region."
  (concat
   (mu4e~main-action-str "\t* [;]Switch focus\n" 'mu4e-context-switch)
   (mu4e~main-action-str "\t* [u]pdate email & database (Alternatively: gr)\n"
                         'mu4e-update-mail-and-index)

   ;; show the queue functions if `smtpmail-queue-dir' is defined
   (if (file-directory-p smtpmail-queue-dir)
       (mu4e~main-view-queue)
     "")
   "\n"

   (mu4e~main-action-str "\t* [N]ews\n" 'mu4e-news)
   (mu4e~main-action-str "\t* [A]bout mu4e\n" 'mu4e-about)
   (mu4e~main-action-str "\t* [H]elp\n" 'mu4e-display-manual)
   (mu4e~main-action-str "\t* [q]uit\n" 'mu4e-quit)))

(defun evil-collection-mu4e-update-main-view ()
  "Update 'Basic' and 'Misc' regions to reflect the new
keybindings."
  (evil-collection-mu4e-replace-region evil-collection-mu4e-new-region-basic
                                       evil-collection-mu4e-begin-region-basic
                                       evil-collection-mu4e-end-region-basic)
  (evil-collection-mu4e-replace-region (evil-collection-mu4e-new-region-misc)
                                       evil-collection-mu4e-begin-region-misc
                                       evil-collection-mu4e-end-region-misc))
