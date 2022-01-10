;;; notmuch-conf.el -- Settings for `notmuch'
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 'notmuch)
(require 'org)
(require 's)

(declare-function pjones:message-convert-to-html "message-conf")

(custom-set-variables
 '(notmuch-show-logo nil)
 '(notmuch-show-all-tags-list t)
 '(notmuch-hello-thousands-separator ",")
 '(notmuch-mua-cite-function #'message-cite-original-without-signature)
 '(notmuch-draft-folder ".Drafts")
 '(notmuch-archive-tags '("-unread" "+archived" "+move"))
 '(notmuch-fcc-dirs ".Sent +sent +from-me")
 '(notmuch-message-replied-tags '("+replied" "+from-me"))
 '(notmuch-message-forwarded-tags '("+forwarded" "+from-me"))
 '(notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
 '(notmuch-saved-searches
   '((:name "Inbox"
      :query "folder:\"\""
      :count-query "folder:\"\" and tag:unread"
      :key "i")
     (:name "Unread"
      :query "tag:unread and not tag:mailing-list"
      :key "u")
     (:name "Flagged"
      :query "tag:flagged"
      :key "f")
     (:name "Drafts"
      :query "tag:draft"
      :key "d")
     (:name "Haskell Cafe"
      :query "tag:haskell-cafe and tag:unread"
      :key "h")
     (:name "Notmuch List"
      :query "tag:notmuch and tag:unread"
      :key "n")
     (:name "Recently Received"
      :query "date:3d..now and (not tag:mailing-list) and (not tag:spam)"
      :key "r")
     (:name "Recently Sent"
      :query "date:3d..now and tag:from-me"
      :key "s")))
 '(notmuch-tagging-keys
  '(("a" notmuch-archive-tags "Archive")
    ("b" ("+blacklisted" "+spam") "Blacklisted")
    ("d" ("+deleted" "+move") "Delete")
    ("f" ("+flagged") "Flag")
    ("r" notmuch-show-mark-read-tags "Mark Read")
    ("s" ("+spam" "+move") "Mark as Spam")
    ("u" ("+unread") "Mark Unread"))))

(let ((map notmuch-search-mode-map))
  (define-key map (kbd "d") #'pjones:notmuch-delete-thread))

(defun pjones:notmuch-tag-thread (&optional tags reverse beg end)
  "Tag selected messages (between BEG and END) with TAGS.
If REVERSE is non-nil then reverse the tagging operation."
  (interactive
   (list
    (notmuch-search-interactive-tag-changes)
    current-prefix-arg
    (notmuch-interactive-region)))
  (let ((query (notmuch-tag-change-list tags reverse)))
    (pcase major-mode
      ('notmuch-search-mode
       (notmuch-search-tag query)
       (when (eq beg end) (notmuch-search-next-thread)))
      ('notmuch-tree-mode
       (notmuch-tree-tag query)
       (notmuch-tree-next-matching-message))
      ('notmuch-show-mode
       (apply #'notmuch-show-tag-message query)
       (unless (notmuch-show-next-open-message) (notmuch-show-next-thread t))))))

(defun pjones:notmuch-delete-thread (&optional undelete beg end)
  "Delete messages between BEG and END.
If UNDELETE is non-nil then reverse the delete operation."
  (interactive (cons current-prefix-arg (notmuch-interactive-region)))
  (let ((tags '("+deleted" "-unread" "+move")))
    (pjones:notmuch-tag-thread tags undelete beg end)))

(defun pjones:notmuch-mark-read (&optional unread beg end)
  "Tag messages between BEG and END as read.
If UNREAD is non-nil, tag them as unread instead."
  (interactive (cons current-prefix-arg (notmuch-interactive-region)))
  (pjones:notmuch-tag-thread '("-unread") unread beg end))

(defun pjones:notmuch-mail-folders (&optional keep)
  "Return a list of valid folder names.

If KEEP is non-nil then don't remove uninteresting folders."
  (let ((not-interesting
         (lambda (file)
           (or (string= ".notmuch" file)
               (string= ".Archive" file)
               (string= ".Sent" file)
               (string= ".Trash" file)
               (string= ".Junk" file))))
        (strip-dot
         (lambda (file)
           (s-chop-prefix "." file))))
    (cons "INBOX"
     (-map strip-dot
      (-remove (if keep (lambda (_) nil) not-interesting)
       (directory-files
        (notmuch-database-path) nil "^\\.[^\\.]"))))))

(defun pjones:notmuch-move-message (&optional only-matched)
  "Move the message under point to another folder.

If ONLY-MATCHED is non-nil only move matched messages."
  (interactive "P")
  (let ((query
         (pcase major-mode
           ('notmuch-search-mode
            (let ((beg (car (notmuch-interactive-region)))
                  (end (cadr (notmuch-interactive-region))))
              (notmuch-search-find-stable-query-region beg end only-matched)))
           ('notmuch-tree-mode
            (notmuch-tree-get-message-id))
           ('notmuch-show-mode
            (notmuch-show-get-message-id))))
        (maildir
         (completing-read
          "Move message(s) to: "
          (pjones:notmuch-mail-folders)
          nil
          t)))
    (start-process "notmuch-refile" nil "notmuch-refile" maildir query)))

;; This variable comes from notmuch-tag.el.
(defvar tag-changes)

(defun pjones:notmuch-after-tag-hook ()
  "Process mail messages after they have been tagged."
  (when (member "+move" tag-changes)
    (message "Refiling tagged messages...")
    (make-process
     :name "x-post-tag"
     :command (list (concat (notmuch-config-get "database.hook_dir") "/x-post-tag"))
     :buffer nil
     :stderr "*x-post-tag*")))

(add-hook 'notmuch-after-tag-hook #'pjones:notmuch-after-tag-hook)
(add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)

;;; notmuch-conf.el ends here
