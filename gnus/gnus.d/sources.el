;;; Mail and News Servers

;; News via NNTP
(setq
 nntp-authinfo-file pmade-authinfo
 gnus-select-method '(nntp "news.gmane.org"))

;; Mail via IMAP and SMTP
(setq
 pmade-mail-server "mail.pmade.com"
 nnimap-authinfo-file pmade-authinfo
 message-send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server pmade-mail-server
 smtpmail-auth-credentials pmade-authinfo
 smtpmail-starttls-credentials `((,pmade-mail-server 25 nil nil))
 smtpmail-local-domain "pmade.com")

(setq gnus-secondary-select-methods
      `((nnimap ,pmade-mail-server (nnimap-stream ssl))))

;; Place sent mail on the server
(setq gnus-message-archive-group "nnimap+mail.pmade.com:INBOX.Sent")

;; Use the correct value for the Message-ID header
(defun message-make-message-id ()
  (concat "<" (message-unique-id) "@pmade.com>"))
