;;; notmuch-conf.el -- Settings for `notmuch'
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'message)
(require 'notmuch)

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
 '(notmuch-archive-tags '("+archived"))
 '(notmuch-fcc-dirs ".Sent +sent +from-me")
 '(notmuch-message-replied-tags '("+replied" "+from-me"))
 '(notmuch-message-forwarded-tags '("+forwarded" "+from-me"))
 '(notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
 '(notmuch-saved-searches
   '((:name "Inbox"
      :query "folder:INBOX"
      :count-query "folder:INBOX and tag:unread"
      :key "i")
     (:name "Unread"
      :query "tag:unread"
      :key "u")
     (:name "Flagged"
      :query "tag:flagged"
      :key "f")
     (:name "Drafts"
      :query "tag:draft"
      :key "d"))))

(add-to-list 'notmuch-tagging-keys
  `(,(kbd "b") ("+blacklisted" "+spam") "Blacklisted"))

(add-to-list 'notmuch-tagging-keys
  `(,(kbd "u") ("+unread") "Mark unread"))

(defmacro pjones:evil-override-notmuch (mode &rest bindings)
  "Override MODE bindings in evil-normal mode with BINDINGS."
  (declare (indent defun))
  `(pjones:evil-override-mode ,mode
     ,@bindings
     "c" #'notmuch-mua-new-mail
     "gR" #'notmuch-poll-and-refresh-this-buffer
     "gr" #'notmuch-refresh-this-buffer
     "H" #'notmuch-help
     "q" #'notmuch-bury-or-kill-this-buffer
     "s" #'notmuch-search
     "'" #'notmuch-jump-search))

(pjones:evil-override-notmuch notmuch-hello-mode)

(pjones:evil-override-notmuch notmuch-search-mode
  "\C-b" #'notmuch-search-scroll-up
  "\C-f" #'notmuch-search-scroll-down
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

(pjones:evil-override-notmuch notmuch-message-mode)

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

(add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)

;;; notmuch-conf.el ends here
