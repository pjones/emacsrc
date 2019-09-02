;;; message-conf.el --- composing mail and news messages
;;; Commentary:
;;; Code:
(require 'company)
(require 'evil-leader)
(require 'message)
(require 'mml)

;; A few extra key bindings:
(evil-leader/set-key-for-mode 'mu4e-compose-mode
  "SPC b" #'message-goto-body
  "SPC c" #'message-goto-cc
  "SPC e" #'mml-secure-message-sign-encrypt
  "SPC f" #'message-goto-from
  "SPC h" #'pjones:mu4e-convert-to-html
  "SPC i" #'message-goto-signature
  "SPC l" #'message-goto-bcc
  "SPC s" #'message-goto-subject
  "SPC t" #'message-goto-to
  "SPC x" #'message-kill-to-signature)

(defun pjones:mu4e-convert-to-html ()
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
  "Configure message mode to my liking."
  ;; Configure completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell))

(add-hook 'message-mode-hook #'pjones:message-mode-hook)

;;; message-conf.el ends here
