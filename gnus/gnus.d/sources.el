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
      gnus-message-archive-group "nnimap+mail.pmade.com:Sent Messages"
      gnus-gcc-mark-as-read t)

(setq gnus-select-method 
  `(nnimap ,pmade-mail-server 
           (nnir-search-engine imap)
           (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
  '((nnimap "imap.googlemail.com"
            (nnimap-stream ssl))))

   ;; gnus-secondary-select-methods
   ;; '((nntp "news.gmane.org")
   ;;   (nntp "news.eternal-september.org"
   ;;         (nntp-open-connection-function nntp-open-tls-stream)
   ;;         (nntp-port-number 563)
   ;;         (nntp-address "news.eternal-september.org"))))

;; Use the correct value for the Message-ID header
(defun message-make-message-id ()
  (concat "<" (message-unique-id) "@pmade.com>"))
