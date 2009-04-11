(eval-when-compile
  (require 'gnus)
  (require 'nnheader))

;; Personal Settings
(setq gnus-ignored-from-addresses
      (regexp-opt
       `(,user-mail-address "mlists@pmade.com" "suv8@pmade.org")))

;; Various Settings
(setq
 message-kill-buffer-on-exit t
 gnus-large-newsgroup 1000
 gnus-topic-display-empty-topics nil
 gnus-treat-date-local 'head
 gnus-read-active-file 'some)           ; Speed up initial load

;; File locations
(setq
 pmade-comm-base "~/.comm-sync"
 pmade-gnus-var (concat pmade-comm-base "/var/gnus")
 pmade-gnus-etc (concat pmade-comm-base "/etc/gnus")
 pmade-sigs-dir (concat pmade-comm-base "/etc/signatures")
 pmade-authinfo (concat pmade-gnus-etc "/authinfo")
 gnus-directory pmade-gnus-var
 mail-signature-dir pmade-sigs-dir
 gnus-agent-directory (concat pmade-gnus-var "/agent")
 gnus-article-save-directory (concat pmade-gnus-var "/news")
 gnus-cache-directory (concat pmade-gnus-var "/cache")
 gnus-startup-file (concat pmade-gnus-etc "/newsrc")
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil)

;; MIME
(eval-after-load "mm-decode"
  '(progn
     (dolist (i '("text/html" "text/richtext" "multipart/mixed" "multipart/related"))
       (add-to-list 'mm-discouraged-alternatives i)
       (setq mm-automatic-display (remove i mm-automatic-display)))))

;; Apple Address Book
(require 'external-abook)
(setq external-abook-command "contacts -lf '%%e\t%%n' %s")

;; Checking for New Mail
(defun pmade-new-level-one-mail () (gnus-group-get-new-news 1))
(gnus-demon-add-handler 'pmade-new-level-one-mail 20 t)
(gnus-demon-init)

;; Highlight the current line in the groups and summary buffers
(defun pmade-gnus-index-hook ()
  "Things to do in any Gnus buffer that is an index (like the
group and summary buffers)"
  (hl-line-mode 1)
  (setq cursor-type nil))

(add-hook 'gnus-group-mode-hook   'pmade-gnus-index-hook)
(add-hook 'gnus-summary-mode-hook 'pmade-gnus-index-hook)

;; User Format Functions
(defun gnus-user-format-function-d (header)
  "Display group and article dates in an easy to read format."
  (let ((date))
    (cond
     (header (setq date (gnus-date-get-time (mail-header-date header))))
     (t      (setq date (gnus-group-timestamp gnus-tmp-group))))
    (if date (format-time-string "%a %b %d, %Y %R" date) "")))

;; Headers
(setq gnus-visible-headers
      '("^From:" "^Subject:" "^To:\\|^[BGF]?Cc:" "^Date:" "^X-Mailer:" "^X-URL:"
        "^Newsgroups:" "^Posted-To:" "^Gnus-Warning:"))
