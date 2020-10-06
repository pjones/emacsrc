;;; notmuch-conf.el -- Settings for `notmuch'
;;
;;; Commentary:
;;
;;; Code:
(require 'dash)
(require 'evil)
(require 'evil-leader)
(require 'notmuch)
(require 's)

(eval-when-compile
  (require 'notmuch-tree)
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

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
      :key "n")))
 '(notmuch-tagging-keys
  '(("a" notmuch-archive-tags "Archive")
    ("b" ("+blacklisted" "+spam") "Blacklisted")
    ("d" ("+deleted" "+move") "Delete")
    ("f" ("+flagged") "Flag")
    ("r" notmuch-show-mark-read-tags "Mark Read")
    ("s" ("+spam" "+move") "Mark as Spam")
    ("u" ("+unread") "Mark Unread"))))

(defmacro pjones:evil-override-notmuch (mode &rest bindings)
  "Override MODE bindings in evil-normal mode with BINDINGS."
  (declare (indent defun))
  `(pjones:evil-override-mode ,mode
     ,@bindings
     "d" #'pjones:notmuch-delete-thread
     "c" #'notmuch-mua-new-mail
     "gR" #'notmuch-poll-and-refresh-this-buffer
     "gr" #'notmuch-refresh-this-buffer
     "H" #'notmuch-help
     "J" #'pjones:notmuch-mark-read
     "M" #'pjones:notmuch-move-message
     "q" #'notmuch-bury-or-kill-this-buffer
     "s" #'notmuch-search
     "'" #'notmuch-jump-search))

(pjones:evil-override-notmuch notmuch-hello-mode)

(pjones:evil-override-notmuch notmuch-search-mode
  "\C-f" #'notmuch-search-scroll-up
  "\C-b" #'notmuch-search-scroll-down
  "G" #'notmuch-search-last-thread
  "gg" #'notmuch-search-first-thread
  "gj" #'notmuch-search-next-thread
  "gk" #'notmuch-search-previous-thread
  "m" #'notmuch-tag-jump
  "S" #'notmuch-search-filter)

(evil-leader/set-key-for-mode 'notmuch-search-mode
  "y t" #'notmuch-search-stash-thread-id
  "y q" #'notmuch-stash-query)

(pjones:evil-override-notmuch notmuch-show-mode
  "^" #'notmuch-show-toggle-visibility-headers
  "gj" #'notmuch-show-next-open-message
  "gk" #'notmuch-show-previous-open-message
  "p" #'notmuch-show-part-map
  "S" #'notmuch-show-filter-thread)

(evil-leader/set-key-for-mode 'notmuch-show-mode
  "y c" #'notmuch-show-stash-cc
  "y d" #'notmuch-show-stash-date
  "y F" #'notmuch-show-stash-filename
  "y f" #'notmuch-show-stash-from
  "y G" #'notmuch-show-stash-git-send-email
  "y i" #'notmuch-show-stash-message-id
  "y I" #'notmuch-show-stash-message-id-stripped
  "y l" #'notmuch-show-stash-mlarchive-link
  "y L" #'notmuch-show-stash-mlarchive-link-and-go
  "y s" #'notmuch-show-stash-subject
  "y T" #'notmuch-show-stash-tags
  "y t" #'notmuch-show-stash-to)

(pjones:evil-override-mode notmuch-message-mode)

(evil-leader/set-key-for-mode 'notmuch-message-mode
  "f s" #'notmuch-draft-save
  "m c" #'message-send-and-exit
  "m k" #'message-kill-buffer
  "m d" #'message-kill-to-signature
  "m e" #'mml-secure-message-sign-encrypt
  "j b" #'message-goto-body
  "j C" #'message-goto-cc
  "j f" #'message-goto-from
  "j i" #'message-goto-signature
  "j B" #'message-goto-bcc
  "j s" #'message-goto-subject
  "j t" #'message-goto-to
  "m h" #'pjones:message-convert-to-html)

(pjones:evil-override-notmuch notmuch-tree-mode
  "gj" #'notmuch-tree-next-matching-message
  "gk" #'notmuch-tree-prev-matching-message
  "M" (notmuch-tree-close-message-pane-and #'notmuch-show-view-raw-message)
  "p" #'notmuch-show-view-all-mime-parts)

(evil-leader/set-key-for-mode 'notmuch-tree-mode
  "y c" #'notmuch-show-stash-cc
  "y d" #'notmuch-show-stash-date
  "y F" #'notmuch-show-stash-filename
  "y f" #'notmuch-show-stash-from
  "y G" #'notmuch-show-stash-git-send-email
  "y i" #'notmuch-show-stash-message-id
  "y I" #'notmuch-show-stash-message-id-stripped
  "y l" #'notmuch-show-stash-mlarchive-link
  "y L" #'notmuch-show-stash-mlarchive-link-and-go
  "y s" #'notmuch-show-stash-subject
  "y T" #'notmuch-show-stash-tags
  "y t" #'notmuch-show-stash-to)

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
    (let ((script (concat (notmuch-database-path) "/.notmuch/hooks/x-post-tag")))
      (start-process "notmuch-x-post-tag" nil script))))

(add-hook 'notmuch-after-tag-hook #'pjones:notmuch-after-tag-hook)
(add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)

;;; notmuch-conf.el ends here
