;;; Mail and News Servers

;; News via NNTP
(setq nntp-authinfo-file pmade-authinfo
      gnus-select-method '(nntp "news.gmane.org"))

;; IMAP (incoming mail)
(setq pmade-mail-server "mail.pmade.com"
      nnimap-authinfo-file pmade-authinfo)

;; SMTP (outgoing mail)
(setq pmade-smtp-host pmade-mail-server
      pmade-smtp-port 25)

;; When I'm on my laptop, do SMTP through a SSH tunnel (because port
;; 25 is blocked by my ISP and at most coffee shops I frequent)
(when (string= "skinny.local" system-name)
  (setq pmade-smtp-host "127.0.0.1"
        pmade-smtp-port 2525
        starttls-extra-arguments '("--insecure")))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server pmade-smtp-host
      smtpmail-smtp-service pmade-smtp-port
      smtpmail-auth-credentials pmade-authinfo
      smtpmail-starttls-credentials `((,pmade-smtp-host ,pmade-smtp-port nil nil))
      smtpmail-local-domain "pmade.com"
      starttls-use-gnutls t)

(setq gnus-secondary-select-methods
      `((nnimap ,pmade-mail-server (nnimap-stream ssl))))

;; Place sent mail on the server
(setq gnus-message-archive-group "nnimap+mail.pmade.com:INBOX.Sent")

;; Use the correct value for the Message-ID header
(defun message-make-message-id ()
  (concat "<" (message-unique-id) "@pmade.com>"))
