;;; wl-conf.el -- Settings for `wanderlust' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Wanderlust is a mail client.  But like Emacs, it's more like a
;; starter kit for those who want to write their own mail client.
;;
;; As you can tell from the size of this configuration file, I'm not a
;; huge fan of the default Wanderlust settings.  I also really dislike
;; the idea of its "~/.folders" file for folder configuration.  Unlike
;; other mail clients, Wanderlust is really easy to hack.
;;
;; Every single bit of my mail client configuration is stored in this
;; file.  There are no other files that I need to sync with other
;; machines that use Wanderlust to read mail.  That's a huge win in my
;; opinion.
;;
;;; Code:

(require 'auth-source)
(require 'dash)
(require 'notifications)
(require 'org) ; For faces.
(require 'signature)
(require 'wl)

(defvar pjones:wl-message-headers
  '("^Date:" "^Subject:" "^From:" "^To:" "^[BGF]?Cc:"
    "^X-Mailer:" "^X-URL:" "^User-Agent:")
  "List of headers to view, in the correct order.")

(defvar pjones:wl-signature-dir
  "~/notes/signatures/"
  "Directory where signatures are stored.")

(custom-set-variables
 '(wl-from "Peter J. Jones <pjones@devalot.com>")
 '(wl-local-domain "pmade.com")
 '(elmo-imap4-default-stream-type 'ssl)
 '(elmo-imap4-default-authenticate-type 'login)
 '(elmo-passwd-storage-type 'auth-source)
 '(elmo-imap4-default-port 993)

 '(wl-folder-desktop-name "Mail")
 '(wl-folder-init-function #'pjones:wl-local-folder-init)
 '(wl-summary-line-format "%T%P %Y-%M-%D %W %h:%m %t %[%17(%c %f%) %] %s")
 '(wl-summary-default-view-alist '(("%inbox" . sequence)))

 '(wl-user-mail-address-regexp
   (rx
    ;; Username:
    "pjones"
    ;; Optional sub-addresses:
    (zero-or-one "+" (one-or-more (not ?@)))
    ;; Username/host separator:
    "@"
    ;; Domains:
    (or "devalot.com" "pmade.com")))

 '(wl-address-file (expand-file-name "~/notes/contacts/wanderlust.txt"))
 '(elmo-msgdb-directory (concat user-emacs-directory "elmo/"))
 '(wl-temporary-file-directory (concat user-emacs-directory "wl-tmp"))
 '(wl-init-file (concat user-emacs-directory "wl/no-such-file"))
 '(wl-folders-file (concat user-emacs-directory "wl/no-such-file"))
 '(wl-alias-file (concat user-emacs-directory "wl/no-such-file"))

 '(wl-dispose-folder-alist
   '(("%Trash:" . remove)
     ("."       . trash)))

 '(wl-message-ignored-field-list '("^.*:"))
 '(wl-message-visible-field-list pjones:wl-message-headers)
 '(wl-message-sort-field-list pjones:wl-message-headers)

 '(wl-generate-mailer-string-function (lambda () "Emacs/Wanderlust"))
 '(wl-draft-always-delete-myself t)
 '(wl-default-draft-cite-header-format-string "%s, %s wrote:\n")
 '(wl-draft-buffer-style 'keep)
 '(wl-draft-reply-buffer-style 'keep)
 '(wl-draft-send-confirm-type 'y-or-n-p)
 '(wl-auto-save-drafts-interval nil)
 '(mime-edit-insert-file-confirm nil)
 '(mime-edit-pgp-signers (list epa-file-encrypt-to))
 '(mime-setup-use-signature nil)
 '(signature-insert-at-eof t)
 '(signature-delete-blank-lines-at-eof t)

 '(wl-draft-config-alist
   '(((string-match "pmade\\.com" wl-draft-parent-folder)
      ("From" . (pjones:wl-generate-from-addr))
      (wl-smtp-posting-server . "mail.pmade.com")
      (wl-smtp-posting-user . (pjones:wl-get-user wl-smtp-posting-server))
      (wl-smtp-posting-port . 465)
      (wl-smtp-authenticate-type . "plain")
      (wl-smtp-connection-type . 'ssl)
      (signature . "devalot"))))

 '(wl-biff-check-interval 120)
 '(wl-biff-notify-hook nil)
 '(wl-biff-new-mail-hook nil)

 '(elmo-message-fetch-confirm nil)
 '(wl-summary-width nil)
 '(wl-summary-showto-folder-regex "%Sent:")
 '(wl-fcc-force-as-read t)
 '(wl-ask-range nil)
 '(wl-default-spec "")
 '(wl-use-folder-petname '(modeline ask-folder read-folder))
 '(wl-demo nil)

 ;; Password caching seems to not work correctly:
 '(password-cache nil))

(custom-set-faces
 '(wl-highlight-action-argument-face ((t (:inherit button))))
 '(wl-highlight-folder-closed-face ((t (:inherit mode-line-inactive))))
 '(wl-highlight-folder-few-face ((t (:inherit warning))))
 '(wl-highlight-folder-killed-face ((t (:inherit mode-line-inactive :strike-through t))))
 '(wl-highlight-folder-many-face ((t (:inherit error))))
 '(wl-highlight-folder-opened-face ((t (:inherit default))))
 '(wl-highlight-folder-path-face ((t (:inherit default))))
 '(wl-highlight-folder-unknown-face ((t (:inherit default))))
 '(wl-highlight-folder-unread-face ((t (:inherit mode-line-emphasis))))
 '(wl-highlight-folder-zero-face ((t (:inherit mode-line-inactive))))
 '(wl-highlight-header-separator-face ((t (:inherit mode-line-highlight))))
 '(wl-highlight-message-citation-header ((t (:inherit org-quote))))
 '(wl-highlight-message-cited-text-1 ((t (:inherit org-level-1))))
 '(wl-highlight-message-cited-text-2 ((t (:inherit org-level-2))))
 '(wl-highlight-message-cited-text-3 ((t (:inherit org-level-3))))
 '(wl-highlight-message-cited-text-4 ((t (:inherit org-level-4))))
 '(wl-highlight-message-cited-text-5 ((t (:inherit org-level-5))))
 '(wl-highlight-message-cited-text-6 ((t (:inherit org-level-6))))
 '(wl-highlight-message-cited-text-7 ((t (:inherit org-level-7))))
 '(wl-highlight-message-cited-text-8 ((t (:inherit org-level-8))))
 '(wl-highlight-message-cited-text-9 ((t (:inherit org-level-1))))
 '(wl-highlight-message-cited-text-10 ((t (:inherit org-level-2))))
 '(wl-highlight-message-header-contents ((t (:inherit org-document-info))))
 '(wl-highlight-message-headers ((t (:inherit default))))
 '(wl-highlight-message-important-header-contents ((t (:inherit org-todo))))
 '(wl-highlight-message-important-header-contents2 ((t (:inherit org-headline-todo))))
 '(wl-highlight-message-signature ((t (:inherit org-cite))))
 '(wl-highlight-message-unimportant-header-contents ((t (:inherit mood-line-unimportant))))
 '(wl-highlight-summary-answered-face ((t (:inherit org-headline-done))))
 '(wl-highlight-summary-copied-face ((t (:inherit default))))
 '(wl-highlight-summary-deleted-face ((t (:inherit mode-line-inactive :strike-through t))))
 '(wl-highlight-summary-displaying-face ((t (:inherit mode-line-highlight))))
 '(wl-highlight-summary-disposed-face ((t (:inherit mode-line-inactive))))
 '(wl-highlight-summary-flagged-face ((t (:inherit org-imminent-deadline))))
 '(wl-highlight-summary-forwarded-face ((t (:inherit default))))
 '(wl-highlight-summary-high-read-face ((t (:inherit default))))
 '(wl-highlight-summary-high-unread-face ((t (:inherit org-headline-todo))))
 '(wl-highlight-summary-important-flag-face ((t (:inherit org-imminent-deadline))))
 '(wl-highlight-summary-killed-face ((t (:inherit mode-line-inactive :strike-through t))))
 '(wl-highlight-summary-low-read-face ((t (:inherit default))))
 '(wl-highlight-summary-low-unread-face ((t (:inherit org-headline-todo))))
 '(wl-highlight-summary-new-face ((t (:inherit org-headline-todo))))
 '(wl-highlight-summary-normal-face ((t (:inherit default))))
 '(wl-highlight-summary-prefetch-face ((t (:inherit default))))
 '(wl-highlight-summary-refiled-face ((t (:inherit mode-line-inactive))))
 '(wl-highlight-summary-resend-face ((t (:inherit default))))
 '(wl-highlight-summary-target-face ((t (:inherit org-agenda-clocking))))
 '(wl-highlight-summary-temp-face ((t (:inherit org-agenda-clocking))))
 '(wl-highlight-summary-thread-top-face ((t (:wight bold))))
 '(wl-highlight-summary-unread-face ((t (:inherit org-headline-todo))))
 '(wl-highlight-thread-indent-face ((t (:inherit mode-line-inactive))))
 '(wl-message-header-narrowing-face ((t (:inherit org-table-header))))
 '(wl-summary-persistent-mark-face ((t (:inherit org-table)))))

(defvar pjones:wl-devalot-folder-names
  '((:name "INBOX"   :nick "Inbox"  :use default :biff t)
    (:name "subs"    :nick "Subs")
    (:name "mlists"  :nick "Lists")
    (:name "Archive" :nick "Archive")
    (:name "Drafts"  :nick "Drafts" :use draft)
    (:name "Sent"    :nick "Sent"   :use fcc)
    (:name "Trash"   :nick "Trash"  :use trash)
    (:name "Queue"   :nick "Queue"  :use queue)
    (:name "Junk"    :nick "Junk")
    (:name "Travel"  :nick "Travel")
    (:name "root"    :nick "Root"))
  "Folders and their attributes for Devalot.")

(defun pjones:wl-get-user (server)
  "Return the user name for SERVER."
  (when-let ((auth (auth-source-search :host server)))
    (plist-get (car auth) :user)))

(defun pjones:wl-devalot-folders-init ()
  "Update `pjones:wl-devalot-folder-names' with Wanderlust entities."
  (let* ((elmo-imap4-default-server "mail.pmade.com")
         (elmo-imap4-default-user (pjones:wl-get-user elmo-imap4-default-server)))
    (setq pjones:wl-devalot-folder-names
          (mapcar
           (lambda (folder) (plist-put folder :wl (pjones:wl-plist-to-imap-folder folder)))
           pjones:wl-devalot-folder-names))
    (mapc #'pjones:wl-set-folder-variables pjones:wl-devalot-folder-names)))

(defun pjones:wl-local-folder-init ()
  "Initialize the folder list.

This builds the Wanderlust folder structure without needing to use a
~/.folders file."
  (setq wl-folder-petname-alist nil
        wl-folder-entity
        (list wl-folder-desktop-name 'group
              (list (pjones:wl-list-to-folders
                     "Devalot" pjones:wl-devalot-folder-names)))))

(defun pjones:wl-list-to-folders (name folders)
  "Return a folder group with NAME using FOLDERS."
  (list name 'group
        (mapcar (lambda (folder)
                  (pjones:wl-folder-append-petname folder)
                  (plist-get folder :wl))
                folders)))

(defun pjones:wl-plist-to-imap-folder (props)
  "Convert a property list to a Wanderlust IMAP folder entity.
PROPS is a property list which looks like `pjones:wl-devalot-folder-names'."
  (concat "%" (plist-get props :name)
          ":\"" elmo-imap4-default-user "\""
          "/" (symbol-name (or elmo-imap4-default-authenticate-type 'clear))
          "@" elmo-imap4-default-server
          ":" (number-to-string (or elmo-imap4-default-port 993))))

(defun pjones:wl-folder-append-petname (folder)
  "Set a folder's nickname from the FOLDER property list."
  (when-let ((entity (plist-get folder :wl))
             (nick (plist-get folder :nick)))
    (wl-folder-append-petname entity nick)))

(defun pjones:wl-set-folder-variables (folder)
  "Set Wanderlust variables based on the given FOLDER."
  (when-let ((entity (plist-get folder :wl)))
    (pcase (plist-get folder :use)
      ('default (setq wl-default-folder entity))
      ('draft   (setq wl-draft-folder entity))
      ('fcc     (setq wl-fcc entity))
      ('trash   (setq wl-trash-folder entity))
      ('queue   (setq wl-queue-folder entity)))
    (when (plist-get folder :biff)
      (add-to-list 'wl-biff-check-folder-list entity))))

(defun pjones:wl-set-folder-variables-for (name)
  "Set all folder variables for Wanderlust folder with NAME."
  (pcase name
    ((pred (string-match-p "pmade\\.com"))
     (mapc #'pjones:wl-set-folder-variables pjones:wl-devalot-folder-names))))

(defun pjones:wl-insert-signature (file)
  "Insert the signature FILE."
  (interactive "F")
  (let ((signature-file-name
          (expand-file-name file (expand-file-name pjones:wl-signature-dir))))
    (save-excursion
      (insert-signature)
      (forward-line -1)
      (newline))))

(unless (assq 'signature wl-draft-config-sub-func-alist)
  (push '(signature . pjones:wl-insert-signature)
        wl-draft-config-sub-func-alist))

(defun pjones:wl-draft ()
  "Start a new draft message with correct variables set.
Notably, ensures that `wl-draft-parent-folder' is set."
  (interactive)
  (when (wl-folder-buffer-group-p)
    (error "Move point to a valid folder and try again!"))
  (let* ((folder (wl-folder-get-entity-from-buffer))
         (name (substring-no-properties folder))
         (id (wl-folder-get-entity-id folder)))
    (pjones:wl-set-folder-variables-for name)
    (funcall-interactively #'wl-draft nil nil nil nil nil name id)))

(defun pjones:wl-draft-prepare-reply ()
  "Clean up a draft reply buffer."
  (when (and (buffer-live-p wl-draft-reply-buffer) wl-draft-reply-buffer)
    (save-excursion
      ;; Remove leading blank lines in the citation, as well as citation
      ;; lines that mention MIME parts:
      (wl-draft-body-goto-top)
      (when (looking-at-p "^On ")
        (forward-line)
        (while (looking-at-p (rx line-start ?> blank
                                 (or line-end
                                     (and ?\[
                                          (one-or-more digit)
                                          (one-or-more blank)))))
          (beginning-of-line)
          (delete-region (point) (progn (forward-visible-line 1) (point)))))
      ;; Kill signatures that appear in the citation:
      (when (search-forward-regexp (rx line-start ?>
                                       (zero-or-more (or blank ?>))
                                       "--" (zero-or-one blank)
                                       line-end) nil t)
        (beginning-of-line)
        (pjones:wl-draft-kill-to-signature)))))

(defun pjones:wl-draft-goto-signature ()
  "Move point to the start of the signature block."
  (interactive)
  (wl-draft-body-goto-bottom)
  (search-backward-regexp (rx line-start "-- " line-end) nil t))

(defun pjones:wl-draft-kill-to-signature ()
  "Kill from point to the start of the signature block."
  (interactive)
  (let ((beg (point))
        (end (save-excursion
               (pjones:wl-draft-goto-signature)
               (point))))
    (if (called-interactively-p 'interactive)
        (kill-region beg end)
      (delete-region beg end))
    (newline)))

(defun pjones:wl-generate-from-addr ()
  "Return a valid From address for the current mail buffer."
  (let ((addrs (if wl-draft-reply-buffer
                   (with-current-buffer wl-draft-reply-buffer
                     (elmo-multiple-fields-body-list '("To" "Cc")))
                 (list wl-from))))
    (or (car (-filter #'wl-address-user-mail-address-p addrs))
        wl-from)))

;; FIXME: It looks like this gets called over and over as long as
;; there is a new message, not just when a new message arrives.
(defun pjones:wl-biff-notify (new-mails)
  "Sent new mail notification.
NEW-MAILS is the number of new mail messages."
  (notifications-notify
   :title "New Mail Messages"
   :body (if (= 1 new-mails) "You have a new mail message."
           (format "You have %d new mail messages." new-mails))
   :app-name "Wanderlust"
   :app-icon "internet-mail"))

(defun pjones:wl-folder-hook ()
  "Customize the Wanderlust folder mode."
  (local-set-key (kbd "g") #'wl-folder-check-current-entity)
  (local-set-key (kbd "j") #'wl-folder-goto-folder)
  (local-set-key (kbd "w") #'pjones:wl-draft))

(defun pjones:wl-summary-hook ()
  "Customize the Wanderlust summary mode."
  (pjones:wl-set-folder-variables-for (wl-summary-buffer-folder-name))
  (hl-line-mode)
  (local-set-key (kbd "a") #'wl-summary-reply-with-citation)
  (local-set-key (kbd "A") (lambda () (interactive) (wl-summary-reply-with-citation t)))
  (local-set-key (kbd "g") #'wl-summary-sync-update)
  (local-set-key (kbd "j") #'wl-summary-goto-folder))

(defun pjones:wl-message-hook ()
  "Customize the Wanderlust message mode."
  (local-set-key (kbd "RET") #'mime-preview-toggle-content))

(defun pjones:wl-draft-hook ()
  "Customize the Wanderlust draft mode."
  (local-set-key (kbd "C-c C-a") #'mime-edit-insert-file)
  (local-set-key (kbd "C-c C-b") #'mail-text)
  (local-set-key (kbd "C-c C-s") #'pjones:wl-draft-kill-to-signature)
  (local-set-key (kbd "C-c C-t") #'wl-addrmgr))

;; Hooks
(add-hook 'mime-view-mode-hook #'pjones:wl-message-hook)
(add-hook 'wl-draft-mode-hook #'pjones:wl-draft-hook)
(add-hook 'wl-folder-mode-hook #'pjones:wl-folder-hook)
(add-hook 'wl-init-hook #'pjones:wl-devalot-folders-init)
(add-hook 'wl-mail-setup-hook #'pjones:wl-draft-prepare-reply)
(add-hook 'wl-mail-setup-hook #'wl-draft-config-exec)
(add-hook 'wl-message-buffer-created-hook #'pjones:wl-message-hook)
(add-hook 'wl-summary-prepared-hook #'pjones:wl-summary-hook)
(remove-hook 'wl-draft-send-hook #'wl-draft-config-exec)

;;; wl-conf.el ends here
