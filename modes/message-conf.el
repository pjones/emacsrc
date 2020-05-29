;;; message-conf.el --- composing mail and news messages
;;; Commentary:
;;; Code:
(require 'message)

(eval-when-compile
  (require 'company)
  (require 'evil-leader)
  (require 'google-contacts-message)
  (require 'mml))

(custom-set-variables
 '(message-signature-directory "~/notes/signatures/"))

;; A few extra key bindings:
(evil-leader/set-key-for-mode 'message-mode
  "m c" #'message-send-and-exit
  "m k" #'message-kill-buffer
  "m d" #'message-kill-to-signature
  "m e" #'mml-secure-message-sign-encrypt
  "m g b" #'message-goto-body
  "m g c" #'message-goto-cc
  "m g f" #'message-goto-from
  "m g i" #'message-goto-signature
  "m g l" #'message-goto-bcc
  "m g s" #'message-goto-subject
  "m g t" #'message-goto-to
  "m h" #'pjones:message-convert-to-html)

(defun pjones:message-convert-to-html ()
  "Turn the current email into HTML using Markdown."
  (interactive)
  (save-excursion
    (message-goto-body)
    (let* ((sig-point
            (save-excursion
              (message-goto-signature)
              (forward-line -1)
              (point)))
           (subject-start
            (save-excursion
              (message-goto-subject)
              (message-beginning-of-line 1)
              (point)))
           (subject-text
            (save-excursion
              (goto-char subject-start)
              (move-end-of-line 1)
              (buffer-substring-no-properties
               subject-start (point))))
           (plain-txt (buffer-substring-no-properties (point) (point-max)))
           (pandoc (concat
                    "pandoc -f markdown -t html -s -V pagetitle:"
                    (shell-quote-argument subject-text))))

      ;; Manipulate the signature so it converts to HTML.
      (save-excursion
        (goto-char sig-point)
        (let ((kill-whole-line nil)) (kill-line))
        (insert "<hr/>")
        (while (re-search-forward "^" nil t)
          (replace-match "| " nil nil)))

      (shell-command-on-region (point) (point-max) pandoc nil t)
      (insert "<#multipart type=alternative>\n")
      (insert plain-txt)
      (insert "<#part type=text/html>\n")
      (exchange-point-and-mark)
      (insert "<#/multipart>\n"))))

(defun pjones:message-mode-hook ()
  "Configure message mode to my liking.")
  ;; Configure completion:
  ;; (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends 'company-ispell))

(add-hook 'message-mode-hook #'pjones:message-mode-hook)

;;; message-conf.el ends here
