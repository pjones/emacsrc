;;; Mail and News Servers

;; IMAP (incoming mail)
(setq pmade-mail-server "mail.pmade.com"
      nnimap-authinfo-file pmade-authinfo)

;; SMTP (outgoing mail)
(setq pmade-smtp-host pmade-mail-server
      pmade-smtp-port 25)

;; When I'm on my laptop, do SMTP through a SSH tunnel (because port
;; 25 is blocked by my ISP and at most coffee shops I frequent)
;;(when (string= "skinny.local" system-name)
  (setq pmade-smtp-host "127.0.0.1"
        pmade-smtp-port 2525
        starttls-extra-arguments '("--insecure"));;)

(setq nntp-authinfo-file pmade-authinfo
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server pmade-smtp-host
      smtpmail-smtp-service pmade-smtp-port
      smtpmail-auth-credentials pmade-authinfo
      smtpmail-starttls-credentials `((,pmade-smtp-host ,pmade-smtp-port nil nil))
      smtpmail-local-domain "pmade.com"
      starttls-use-gnutls t
      gnus-message-archive-group "nnimap+mail.pmade.com:INBOX.Sent")

(setq gnus-select-method `(nnimap ,pmade-mail-server (nnimap-stream ssl))
      gnus-secondary-select-methods
      '((nntp "news.gmane.org")
        (nntp "news.motzarella.org"
              (nntp-open-connection-function nntp-open-tls-stream)
              (nntp-port-number 563)
              (nntp-address "news.motzarella.org"))))

;; Use the correct value for the Message-ID header
(defun message-make-message-id ()
  (concat "<" (message-unique-id) "@pmade.com>"))
