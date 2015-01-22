;;; Mail and News Servers
(setq pmade-mail-server "mail.pmade.com"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server pmade-mail-server
      smtpmail-smtp-service 465
      smtpmail-stream-type 'tls
      smtpmail-local-domain "pmade.com"
      starttls-use-gnutls t
      starttls-extra-arguments '("--insecure"))

(setq gnus-select-method
  `(nnimap "pmade"
           (nnimap-address ,pmade-mail-server)
           (nnimap-server-port 993)
           (nnimap-authenticator plain)
           (nnimap-stream tls)
           (nnir-search-engine imap)))

(setq gnus-secondary-select-methods
  '((nnimap "SCORS"
      (nnimap-address "scowa.sc.gov")
      (nnimap-server-port 993)
      (nnimap-authenticator login)
      (nnimap-stream tls))
    (nntp "Gmane"
      (nntp-address "news.gmane.org"))))

(setq gnus-message-archive-group "nnimap+mail.pmade.com:Sent Messages"
      gnus-message-archive-method
        '(nnfolder "archive"
           (nnfolder-directory   "~/.gnus.d/archive")
           (nnfolder-active-file "~/.gnus.d/archive/active")
           (nnfolder-get-new-mail nil)
           (nnfolder-inhibit-expiry t)))

;; Use the correct value for the Message-ID header
(defun message-make-message-id ()
  (concat "<" (message-unique-id) "@pmade.com>"))

;; When I can't access my mail server because some really stupid
;; networks redirect traffic to port 25.
(defun pmade:gnus-use-ssh-tunnel ()
  "Send outgoing mail over a SSH tunnel."
  (interactive)
  (setq smtpmail-smtp-server "127.0.0.1"
        smtpmail-smtp-service 2525
        smtpmail-starttls-credentials `((,smtpmail-smtp-server ,smtpmail-smtp-service nil nil))
        starttls-extra-arguments '("--insecure")))
