;;; gnus-conf.el -- Settings for `gnus' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'gnus)
(require 'gnus-art)
(require 'message)
(require 's)

(defvar gnus-tmp-group nil
  "Variable from gnus-group.el.
Declared here to avoid compiler warnings.")

(defvar gnus-tmp-decoded-group nil
  "Variable from gnus-group.el.
Declared here to avoid compiler warnings.")

(custom-set-variables
 ;; Basic Gnus settings:
 '(gnus-startup-file "~/notes/bookmarks/gnus")
 '(gnus-read-newsrc-file nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-activate-level 3)
 '(gnus-process-mark ?✯)
 '(gnus-large-newsgroup 800)
 '(gnus-save-score t)
 '(gnus-novice-user nil)
 '(gnus-expert-user t)
 '(gnus-interactive-exit nil)
 '(gnus-extract-address-components 'mail-extract-address-components)
 '(gnus-use-full-window t)
 '(gnus-always-force-window-configuration t)

 ;; Gnus Agent (gnus-agent.el):
 '(gnus-agent t)
 '(gnus-agent-handle-level 2)
 '(gnus-agent-go-online t)

 ;; Gnus Article (gnus-art.el):
 '(gnus-article-update-date-headers 60)
 '(gnus-inhibit-images t)
 '(gnus-treat-body-boundary 'head)
 '(gnus-treat-fill-long-lines t)
 '(gnus-treat-from-gravatar 'head)
 '(gnus-treat-leading-whitespace t)
 '(gnus-treat-mail-gravatar 'head)
 '(gnus-treat-strip-leading-blank-lines  t)
 '(gnus-treat-strip-multiple-blank-lines t)
 '(gnus-treat-strip-trailing-blank-lines t)
 '(gnus-treat-unsplit-urls t)
 '(gnus-treat-x-pgp-sig 'head)
 '(gnus-visible-headers
   '("^From:" "^Subject:" "^To:" "^[BGF]?Cc:" "^Date:" "^X-Mailer:"
     "^X-URL:" "^Newsgroups:" "^Posted-To:" "^Gnus-Warning:"))

 ;; Gnus Asynchronous (gnus-async.el):
 '(gnus-asynchronous t)

 ;; Gnus Group:
 '(gnus-permanently-visible-groups nil)
 '(gnus-new-mail-mark ?✉)
 '(gnus-group-line-format
   "%B%P%p%m%M %7{%5y%}: %*%uc %-30= (%g)%-60= %5{%S%L%}\n")

 ;; Gnus Message:
 '(gnus-gcc-mark-as-read t)
 '(gnus-message-replysign t)
 '(gnus-message-replyencrypt t)
 '(gnus-message-replysignencrypted t)

 ;; Gnus Summary:
 '(gnus-preserve-marks nil)
 '(gnus-unread-mark ?✉)
 '(gnus-ticked-mark ?⚡)
 '(gnus-dormant-mark ?☒)
 '(gnus-del-mark ?✗)
 '(gnus-read-mark ? )
 '(gnus-expirable-mark ?⌛)
 '(gnus-killed-mark ?☠)
 '(gnus-catchup-mark ? )
 '(gnus-replied-mark ?↶)
 '(gnus-forwarded-mark ?→)
 '(gnus-recent-mark ? )
 '(gnus-cached-mark ?⚓)
 '(gnus-unseen-mark ? )
 '(gnus-ancient-mark ? )
 '(gnus-canceled-mark ?⊘)
 '(gnus-summary-stop-at-end-of-message t)
 '(gnus-summary-make-false-root 'adopt)
 '(gnus-auto-center-summary nil)
 '(gnus-sum-thread-tree-indent " ")
 '(gnus-sum-thread-tree-root "")
 '(gnus-sum-thread-tree-false-root "")
 '(gnus-sum-thread-tree-single-indent "")
 '(gnus-sum-thread-tree-vertical    "│")
 '(gnus-sum-thread-tree-leaf-with-other "├─► ")
 '(gnus-sum-thread-tree-single-leaf   "╰─► ")
 '(gnus-summary-to-prefix "〉 ")
 '(gnus-summary-line-format
   "%5{%z %U %R %}%-6=%f%-25= [%ud] %B%*%s\n")
 '(gnus-summary-highlight
   '(((eq mark gnus-canceled-mark)
      . gnus-summary-cancelled)
     ((or (eq mark gnus-dormant-mark)
          (eq mark gnus-ticked-mark))
      . gnus-summary-normal-ticked)
     ((eq mark gnus-unread-mark)
      . gnus-summary-normal-unread)
     (t
      . gnus-summary-normal-read)))

 ;; Gnus Topic:
 '(gnus-topic-indent-level 2)
 '(gnus-topic-display-empty-topics nil)

 ;; Accounts
 '(gnus-ignored-newsgroups ; Needed for gmail to work:
   "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

 '(gnus-nntp-server nil)
 '(gnus-select-method '(nnnil ""))

 '(gnus-secondary-select-methods
   '((nnimap "Devalot"
       (nnimap-address "imap.fastmail.com")
       (nnimap-server-port 993)
       (nnimap-authenticator plain)
       (nnimap-stream tls)
       (nnmail-expiry-target "nnimap+Devalot:Trash"))
     (nnimap "WGU"
       (nnimap-address "imap.gmail.com")
       (nnimap-server-port 993)
       (nnimap-authenticator plain)
       (nnimap-stream tls)
       (nnir-search-engine imap)
       (nnmail-expiry-target "nnimap+WGU:[Gmail]/Trash"))))

 `(gnus-posting-styles
   '((".*"
      (name "Peter J. Jones")
      (address ,(concat "pjones" "@" "devalot.com"))
      (signature :file "devalot")
      (eval (setq gnus-message-archive-group "nnimap+Devalot:Sent"
                  smtpmail-smtp-server "smtp.fastmail.com"
                  smtpmail-smtp-service 465
                  smtpmail-stream-type 'ssl)))
     ("WGU"
      (name "Peter J. Jones")
      (address ,(concat "pjon409" "@" "wgu.edu"))
      (signature :file "wgu")
      (eval (setq gnus-message-archive-group "nnimap+WGU:[Gmail]/Sent"
                  smtpmail-smtp-server "smtp.gmail.com"
                  smtpmail-smtp-service 465
                  smtpmail-stream-type 'ssl)))))

 '(gnus-parameters
   '(("^nnimap\\+\\w+:\\(.+\\)$"
       (comment . "\\1")
       (agent-predicate . true)
       (agent-enable-expiration t)
       (agent-enable-undownloaded-faces t))
     (":\\[Gmail\\]/\\(\\w+\\)"
       (comment . "\\1"))
     (":INBOX"
       (comment . "Inbox")
       (gnus-show-threads nil))
     (":\\(INBOX\\|Archive\\|Sent\\)"
       (display . all)
       (gnus-article-sort-functions '(gnus-article-sort-by-date)))
     (":mlists"
       (comment . "Mailing Lists")
       (auto-expire . t)
       (expiry-wait . 14)
       (display . [unread])
       (gnus-thread-hide-subtree t))
     (":subs"
       (comment . "Sub Addrs")
       (display . [unread]))))

 ;; Sending mail:
 '(message-send-mail-function #'smtpmail-send-it)
 '(smtpmail-queue-dir "~/.cache/smtpmail/queue")
 '(mml-secure-openpgp-encrypt-to-self t)
 '(mml-secure-smime-encrypt-to-self t))

;;; Faces:
(defvar gnus-face-1 font-lock-builtin-face)
(defvar gnus-face-2 font-lock-constant-face)
(defvar gnus-face-3 font-lock-function-name-face)
(defvar gnus-face-4 font-lock-keyword-face)
(defvar gnus-face-5 font-lock-negation-char-face)
(defvar gnus-face-6 font-lock-string-face)
(defvar gnus-face-7 font-lock-type-face)
(defvar gnus-face-8 font-lock-variable-name-face)
(defvar gnus-face-9 font-lock-comment-face)

;;; Functions
(defun pjones:gnus-format-date (header)
  "Format dates for the Gnus header HEADER."
  (let* ((base-fmt "%m/%d/%y")
         (date (cond
                (header (gnus-date-get-time (mail-header-date header)))
                (t (current-time))))
         (since (if date
                    (time-to-number-of-days (time-since date))
                  0.0))
         (fmt (cond
               ((> since 1.0) (concat base-fmt " %a  "))
               (t (concat base-fmt " %R")))))
    (if date (format-time-string fmt date) "")))
(defalias 'gnus-user-format-function-d #'pjones:gnus-format-date)

(defun pjones:gnus-format-comment (&rest _dummy)
  "Render a group's comment.
Why and I doing this?  Well, when Gnus renders the group line in the
group buffer using the %C specifier it can't find the `comment'
parameter.  I'm not sure why it's not in the list of parameters
returned by `gnus-group-get-parameter'.  It does show up in
`gnus-group-find-parameter` though."
  (let ((group (or gnus-tmp-group (gnus-group-group-name))))
    (if group
        (cond
         ((s-matches-p "^nnrss:" group)
          (s-replace-regexp "^nnrss:" "" group))
         (t
          (or (gnus-group-find-parameter group 'comment)
              (and
               gnus-tmp-decoded-group
               (gnus-short-group-name gnus-tmp-decoded-group))
              group)))
      "Missing comment and group name")))
(defalias 'gnus-user-format-function-c #'pjones:gnus-format-comment)

(defmacro pjones:gnus-article-move-to (group)
  "Move marked/current article to GROUP."
  `(defun ,(intern (concat "pjones:gnus-article-move-to-" group)) ()
    ,(concat "Move article(s) to " group)
    (interactive)
    ;; gnus-newsgroup-name
    ;; If group has a : remove the trailing word with
    (let ((dest
          (if (s-matches-p ":" gnus-newsgroup-name)
              (s-replace-regexp
               ":.*$"
               (concat ":" ,group)
               gnus-newsgroup-name t)
            ,group)))
      (gnus-summary-move-article nil dest))))

(defmacro pjones:gnus-summary-reverse-sort-by (func)
  "Generate reverse sorting function from FUNC."
  `(defun ,(intern (concat "pjones:" (symbol-name func) "-rev")) ()
     ,(concat
       "Reverse sort by "
       (car (last (s-split "-" (symbol-name func)))))
     (interactive)
     (let ((current-prefix-arg '(4)))
       (call-interactively (quote ,func)))))

(defun pjones:gnus-demon-init ()
  "Prepare the Gnus Demon."
  (gnus-demon-add-handler 'gnus-demon-scan-mail 2 30))

(defun pjones:gnus-summary-header-line ()
  "Set `header-line-format' for `gnus-summary-mode'."
  (setq header-line-format
        '((:propertize
           gnus-newsgroup-name
           face gnus-header-content))))

;;; Key bindings:
(let ((map gnus-group-mode-map))
  (define-key map (kbd "m") (lambda () (interactive) (gnus-group-mail '(4)))))

(let ((map gnus-summary-mode-map))
  (define-key map (kbd "s") nil)
  (define-key map (kbd "r") #'gnus-article-wide-reply-with-original)
  (define-key map (kbd "R") #'gnus-article-reply-with-original)
  (define-key map (kbd "s o") #'gnus-summary-sort-by-original)
  (define-key map (kbd "s d") #'gnus-summary-sort-by-date)
  (define-key map (kbd "s a") #'gnus-summary-sort-by-author)
  (define-key map (kbd "s s") #'gnus-summary-sort-by-subject)
  (define-key map (kbd "s t") #'gnus-summary-sort-by-recipient)
  (define-key map (kbd "s D") (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-date))
  (define-key map (kbd "s A") (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-author))
  (define-key map (kbd "s S") (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-subject))
  (define-key map (kbd "s T") (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-recipient))
  (define-key map (kbd "v a") (pjones:gnus-article-move-to "Archive"))
  (define-key map (kbd "v d") (pjones:gnus-article-move-to "Trash"))
  (define-key map (kbd "v i") (pjones:gnus-article-move-to "INBOX"))
  (define-key map (kbd "v r") (pjones:gnus-article-move-to "Review"))
  (define-key map (kbd "v s") (pjones:gnus-article-move-to "Spam"))
  (define-key map (kbd "v t") (pjones:gnus-article-move-to "Trash")))

(let ((map gnus-article-mode-map))
  (define-key map (kbd "r") #'gnus-article-wide-reply-with-original)
  (define-key map (kbd "R") #'gnus-article-reply-with-original))

;;; Hooks:
(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook #'hl-line-mode)
(add-hook 'gnus-started-hook #'gnus-delay-initialize)
(add-hook 'gnus-started-hook #'pjones:gnus-demon-init)
(add-hook 'gnus-summary-mode-hook #'hl-line-mode)
(add-hook 'gnus-summary-mode-hook #'pjones:gnus-summary-header-line)

;;; gnus-conf.el ends here
