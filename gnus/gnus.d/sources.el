;;; Mail and News Servers

;; IMAP (incoming mail)
(setq pmade-mail-server "mail.pmade.com")

;; SMTP (outgoing mail)
(setq pmade-smtp-host pmade-mail-server
      pmade-smtp-port 25)

;; When I'm on my laptop, do SMTP through a SSH tunnel (because port
;; 25 is blocked by my ISP and at most coffee shops I frequent)
;;(when (string= "skinny.local" system-name)
  ;; (setq pmade-smtp-host "127.0.0.1"
  ;;       pmade-smtp-port 2525
  ;;       starttls-extra-arguments '("--insecure"));;)
(setq pmade-smtp-host pmade-mail-server
      pmade-smtp-port 25
      starttls-extra-arguments '("--insecure"))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server pmade-smtp-host
      smtpmail-smtp-service pmade-smtp-port
      smtpmail-starttls-credentials `((,pmade-smtp-host ,pmade-smtp-port nil nil))
      smtpmail-local-domain "pmade.com"
      starttls-use-gnutls t
      gnus-gcc-mark-as-read t)

(setq gnus-select-method
  `(nnimap ,pmade-mail-server
           (nnir-search-engine imap)
           (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
  '((nnimap "SCORS"
      (nnimap-address "scowa.sc.gov")
      (nnimap-server-port 993)
      (nnimap-authenticator login)
      (nnimap-stream ssl))
    (nntp "Gmane"
      (nntp-address "news.gmane.org"))))

;; (nnimap "Thrive Smart"
;;       (nnimap-address "tsmail.pmade.com")
;;       (nnimap-server-port 993)
;;       (nnimap-stream ssl))
;;     (nnimap "Aura Software"
;;       (nnimap-address "asmail.pmade.com")
;;       (nnimap-server-port 993)
;;       (nnimap-stream ssl))


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
