;;; gnus-conf.el -- Configuration for Gnus
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'gnus)
(require 's)
(require 'dianyou)

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(defvar gnus-tmp-group nil
  "Variable from gnus-group.el.
Declared here to avoid compiler warnings.")

(defvar gnus-tmp-decoded-group nil
  "Variable from gnus-group.el.
Declared here to avoid compiler warnings.")

(custom-set-variables
 ;; Basic Gnus settings:
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
 '(gnus-parameters
   '(("^nnimap"
      (agent-predicate . true)
      (agent-enable-expiration t)
      (agent-enable-undownloaded-faces t))
     (":INBOX$"
      (comment . "Inbox")
      (gnus-show-threads nil))
     (":\\(INBOX\\|Archive\\|Sent\\)$"
      (display . all)
      (gnus-article-sort-functions '(gnus-article-sort-by-date)))
     (":mlists$"
      (comment . "Mailing Lists")
      (auto-expire . t)
      (expiry-wait . 14)
      (display . [unread])
      (gnus-thread-hide-subtree t))
     (":subs$"
      (comment . "Sub Addrs")
      (display . [unread]))
     ("^nnrss:"
      (display . [unread])
      (gnus-summary-line-format "[%ud] %B%*%s\n"))))

 ;; Gnus Message:
 '(gnus-gcc-mark-as-read t)
 '(gnus-message-replysign t)
 '(gnus-message-replyencrypt t)
 '(gnus-message-replysignencrypted t)
 `(gnus-posting-styles
   '((".*"
      (name "Peter Jones")
      (address ,(concat "pjones" "@" "devalot.com"))
      (signature :file "devalot")
      (gcc "nnimap+devalot:Sent")
      (eval (setq smtpmail-smtp-server "mail.pmade.com"
                  smtpmail-smtp-service 465
                  smtpmail-stream-type 'ssl)))
     ("outlook\\.office365\\.com"
      (address ,(concat "peter.jones" "@" "rfa.sc.gov"))
      (signature :file "rfa")
      (gcc "nnimap+rfa:Sent")
      (eval (setq smtpmail-smtp-server "outlook.office365.com"
                  smtpmail-smtp-service 587
                  smtpmail-stream-type 'starttls)))))

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

 '(gnus-nntp-server nil)
 '(gnus-select-method
   '(nnrss "rss"))
 '(gnus-secondary-select-methods
   '((nnimap "devalot"
             (nnimap-address "mail.pmade.com")
             (nnimap-server-port 993)
             (nnimap-authenticator plain)
             (nnimap-stream tls))
     (nnimap "rfa"
             (nnimap-address "outlook.office365.com")
             (nnimap-server-port 993)
             (nnimap-authenticator plain)
             (nnimap-stream tls))))

 ;; How to "expire" articles:
 '(nnmail-expiry-target #'pjones:gnus-trash-target)

 ;; Sending mail:
 '(message-send-mail-function 'smtpmail-send-it)
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

(defun pjones:gnus-trash-target (group)
  "Get the GROUP target for expired messages."
  (s-replace-regexp ":.*$" ":Trash" group))

(defun pjones:gnus-summary-close-or-quit ()
  "Close the article buffer, or kill the summary buffer."
  (interactive)
  (if (get-buffer-window gnus-article-buffer)
      (gnus-configure-windows 'summary 'force)
    (gnus-summary-exit)))

(defun pjones:gnus-summary-show-or-select ()
  "Show or select the article buffer."
  (interactive)
  (if (get-buffer-window gnus-article-buffer)
      (gnus-summary-select-article-buffer)
    (gnus-summary-show-article)))

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
(pjones:evil-override-mode gnus-group-mode
  "a" (lambda () (interactive) (gnus-group-mail '(4)))
  "c"  #'gnus-group-catchup-current
  "l"  #'gnus-group-list-groups
  "L"  #'gnus-group-list-all-groups
  "gr" #'gnus-group-get-new-news
  "gR" #'gnus-group-get-new-news-this-group
  "g/" #'dianyou-group-make-nnir-group)

(pjones:evil-override-mode gnus-summary-mode
  "q" #'pjones:gnus-summary-close-or-quit
  "r" #'gnus-article-wide-reply-with-original
  "R" #'gnus-article-reply-with-original
  "m" #'gnus-summary-mark-as-processable
  "gr" #'gnus-summary-rescan-group
  "gx" #'gnus-article-browse-html-article
  "+" #'gnus-summary-tick-article
  "-" #'gnus-summary-mark-as-read-forward
  "oo" #'gnus-summary-sort-by-original
  "od" #'gnus-summary-sort-by-date
  "oa" #'gnus-summary-sort-by-author
  "os" #'gnus-summary-sort-by-subject
  "ot" #'gnus-summary-sort-by-recipient
  "oD" (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-date)
  "oA" (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-author)
  "oS" (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-subject)
  "oT" (pjones:gnus-summary-reverse-sort-by gnus-summary-sort-by-recipient)
  (kbd "<tab>") #'gnus-summary-select-article-buffer)

(evil-leader/set-key-for-mode 'gnus-summary-mode
  "m a" (pjones:gnus-article-move-to "Archive")
  "m d" (pjones:gnus-article-move-to "Trash")
  "m i" (pjones:gnus-article-move-to "INBOX")
  "m j" (pjones:gnus-article-move-to "Junk")
  "m r" (pjones:gnus-article-move-to "Review")
  "m t" (pjones:gnus-article-move-to "Trash")
  "m m" #'gnus-summary-move-article
  "m /" gnus-summary-limit-map)

(pjones:evil-override-mode gnus-article-mode
  "r" #'gnus-article-wide-reply-with-original
  "R" #'gnus-article-reply-with-original
  "q" #'pjones:gnus-summary-close-or-quit)

;;; Hooks:
(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook #'hl-line-mode)
(add-hook 'gnus-started-hook #'gnus-delay-initialize)
(add-hook 'gnus-started-hook #'pjones:gnus-demon-init)
(add-hook 'gnus-summary-mode-hook #'hl-line-mode)
(add-hook 'gnus-summary-mode-hook #'pjones:gnus-summary-header-line)

;;; gnus-conf.el ends here
