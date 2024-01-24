;;; message-conf.el -- Settings for `message' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'message)
(require 'mml)
(require 'org-mime)
(require 'smtpmail)

(custom-set-variables
 '(message-confirm-send t)
 '(message-send-mail-function #'smtpmail-send-it)
 '(message-directory "~/mail")
 '(message-from-style 'angles)
 '(message-citation-line-function #'message-insert-formatted-citation-line)
 '(message-citation-line-format "On %a, %b %d %Y, %N wrote:")
 '(message-cite-reply-position 'traditional)
 '(message-auto-save-directory nil)
 '(message-dont-reply-to-names '("pjones@pmade.com" "pmadeinc@gmail.com"))
 '(message-kill-buffer-on-exit t)
 '(message-signature-directory "~/notes/signatures/")
 `(mml-secure-key-preferences
   '((OpenPGP
      (sign ("pjones@devalot.com" ,epa-file-encrypt-to))
      (encrypt ("pjones@devalot.com" ,epa-file-encrypt-to))))))

;; A few extra key bindings:
(let ((map message-mode-map))
  (define-key map (kbd "C-c C-b") #'message-goto-body)
  (define-key map (kbd "C-c C-c") #'message-send-and-exit)
  (define-key map (kbd "C-c C-e") #'mml-secure-message-sign-encrypt)
  (define-key map (kbd "C-c C-f") #'message-goto-from)
  (define-key map (kbd "C-c C-h") #'org-mime-htmlize)
  (define-key map (kbd "C-c C-k") #'message-kill-buffer)
  (define-key map (kbd "C-c C-s") #'message-kill-to-signature)
  (define-key map (kbd "C-c C-S-b") #'message-goto-bcc)
  (define-key map (kbd "C-c C-S-c") #'message-goto-cc)
  (define-key map (kbd "C-c C-S-s") #'message-goto-signature)
  (define-key map (kbd "C-c C-s") #'message-goto-subject)
  (define-key map (kbd "C-c C-t") #'message-goto-to))

;;; message-conf.el ends here
