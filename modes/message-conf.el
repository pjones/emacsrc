;;; message-conf.el -- Settings for `message' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'message)
(require 'mml)
(require 'org-mime)
(require 'smtpmail)

(defvar pjones:message-htmlize-before-send t
  "When non-nil, convert message to HTML before sending.")

(defun pjones:toggle-message-htmlize-before-send ()
  "Toggle the value of `pjones:message-htmlize-before-send'."
  (interactive)
  (setq-local pjones:message-htmlize-before-send
              (not pjones:message-htmlize-before-send))
  (message "HTML on send is now %s" pjones:message-htmlize-before-send))

(defun pjones:message-send-hook ()
  "Hook function for `message-send-hook'."
  (when pjones:message-htmlize-before-send
    (org-mime-htmlize)))

(custom-set-variables
 '(message-confirm-send t)
 '(message-send-mail-function #'smtpmail-send-it)
 '(message-directory "~/mail")
 '(message-from-style 'angles)
 '(message-citation-line-function #'message-insert-formatted-citation-line)
 '(message-citation-line-format "On %a, %b %d %Y, %N wrote:")
 '(message-cite-reply-position 'traditional)
 '(message-dont-reply-to-names '("pjones@pmade.com" "pmadeinc@gmail.com"))
 '(message-kill-buffer-on-exit t)
 '(message-signature-directory "~/notes/signatures/")
 `(mml-secure-key-preferences
   '((OpenPGP
      (sign ("pjones@devalot.com" ,epa-file-encrypt-to))
      (encrypt ("pjones@devalot.com" ,epa-file-encrypt-to))))))

;; A few extra key bindings:
(let ((map message-mode-map))
  (define-key map (kbd "C-c '") #'org-mime-edit-mail-in-org-mode)
  (define-key map (kbd "C-c C-b") #'message-goto-body)
  (define-key map (kbd "C-c C-c") #'message-send-and-exit)
  (define-key map (kbd "C-c C-e") #'mml-secure-message-sign-encrypt)
  (define-key map (kbd "C-c C-f") #'message-goto-from)
  (define-key map (kbd "C-c C-h") #'pjones:toggle-message-htmlize-before-send)
  (define-key map (kbd "C-c C-k") #'message-kill-buffer)
  (define-key map (kbd "C-c C-s") #'message-goto-subject)
  (define-key map (kbd "C-c C-s") #'message-kill-to-signature)
  (define-key map (kbd "C-c C-S-b") #'message-goto-bcc)
  (define-key map (kbd "C-c C-S-c") #'message-goto-cc)
  (define-key map (kbd "C-c C-S-s") #'message-goto-signature)
  (define-key map (kbd "C-c C-t") #'message-goto-to))

(add-to-list 'message-send-hook #'pjones:message-send-hook)

;;; message-conf.el ends here
