;;; mu4e-conf.el -- Settings for `mu4e' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'json)
(require 'message)
(require 'mu4e)
(require 'rx)

(declare-function orgalist-mode "orgalist")
(declare-function pjones:indent-or-complete "../lisp/completion.el")
(declare-function yas-minor-mode "yasnippet")

(defun pjones:mu4e-match-func (prefix msg)
  "Return non-nil if MSG is in the PREFIX maildir."
  (when msg
    (string-match-p
     (rx bol "/" (literal prefix) "/")
     (mu4e-message-field msg :maildir))))

(defun pjones:mu4e-contexts ()
  "Generate the `mu4e-context' variable contents from JSON.
The JSON document comes from my tilde project."
  (when-let* ((file-name (expand-file-name "~/.config/tilde/mail.json"))
              (json (and (file-exists-p file-name)
                         (json-read-file file-name))))
    (mapcar
     (lambda (account)
       (let* ((name (symbol-name (car account)))
              (domain (car (seq-filter
                            (lambda (domain)
                              (not (equal :json-false (alist-get 'default domain))))
                            (alist-get 'domains (cdr account)))))
              (user (seq-first (alist-get 'users (cdr domain)))))
         (make-mu4e-context
          :name name
          :match-func (apply-partially #'pjones:mu4e-match-func name)
          :vars `((user-mail-address  . ,(concat user "@" (symbol-name (car domain))))
                  (mu4e-sent-folder   . ,(concat "/" name "/Sent"))
                  (mu4e-drafts-folder . ,(concat "/" name "/Drafts"))
                  (mu4e-trash-folder  . ,(concat "/" name "/Trash"))
                  (mu4e-refile-folder . ,(concat "/" name "/Archive"))))))
       json)))

(defun pjones:mu4e-personal-addresses-re ()
  "Return a regular expression that matches any address."
  (let ((addrs (mu4e-personal-addresses))
        (case-fold-search t))
    (string-join
     (seq-map (lambda (addr)
                (cond
                 ((string-match-p "\\+" addr) addr)
                 ((string-match (rx bol (group (1+ (not ?@))) ?@
                                    (group (1+ anychar))) addr)
                  (concat (match-string 1 addr)
                          (rx (opt "+" (1+ (not ?@)))) "@"
                          (match-string 2 addr)))
                 (t addr)))
              (append addrs '("[^@]+@fastmail.com")))
     "\\|")))

(defvar pjones:mu4e-secret-drafts-folder
  (concat (concat (expand-file-name "~/mail/") "mu-secret-drafts/"))
  "Maildir where encrypted messages are saved.")

(defun pjones:mu4e-compose-secret-drafts ()
  "Ensure encrypted messages are not written to the drafts folder."
  (save-excursion
    (message-goto-body)
    (when-let* (((search-forward-regexp
                  (rx "<#secure "
                      (1+ not-newline)
                      "encrypt"
                      (0+ not-newline)
                      ">") nil t))
                (file-name (buffer-file-name))
                (dir (file-name-directory file-name))
                (base (file-name-base file-name))
                (drafts (concat pjones:mu4e-secret-drafts-folder "cur/"))
                ((not (string= drafts dir)))
                (new-name (concat drafts base)))
      (when (not (file-exists-p pjones:mu4e-secret-drafts-folder))
        (make-directory
         (file-name-directory
          (directory-file-name pjones:mu4e-secret-drafts-folder)) t)
        (call-process "mu" nil nil nil "mkdir" pjones:mu4e-secret-drafts-folder)
        (call-process "touch" nil nil nil (concat pjones:mu4e-secret-drafts-folder ".noindex")))
      (let ((change-major-mode-with-file-name nil))
        (rename-file file-name new-name)
        (set-visited-file-name new-name nil t)))))

(defun pjones:mu4e-compose-mode-hook ()
  "Prepare a new compose buffer."
  ;; Bindings:
  (keymap-local-set "TAB" #'pjones:indent-or-complete)

  ;; Enable other modes:
  (yas-minor-mode)
  (orgalist-mode)

  ;; Always make sure we use the correct drafts folder.
  (add-hook 'before-save-hook #'pjones:mu4e-compose-secret-drafts nil t)

  ;; Set the From address to one of my email addresses, the one that
  ;; was used in the email that is being replied to.
  (when mu4e-compose-parent-message
    (when-let* ((regex (pjones:mu4e-personal-addresses-re))
                (contacts (append
                           (mu4e-message-field mu4e-compose-parent-message :to)
                           (mu4e-message-field mu4e-compose-parent-message :cc)
                           (mu4e-message-field mu4e-compose-parent-message :bcc)
                           (mu4e-message-field mu4e-compose-parent-message :from)))
                (match (seq-find
                        (lambda (contact)
                          (string-match-p regex (mu4e-contact-email contact)))
                        contacts))
                (from (message-make-from user-full-name
                                         (mu4e-contact-email match))))
      (save-excursion
        (save-restriction
          (message-narrow-to-headers-or-head)
          (message-remove-header "From")
          (goto-char (point-min))
          (insert
           (concat "From: " from "\n"))))))

  ;; Try to find the correct signature using the sources:
  ;; 1. A file matching the domain of the From address.
  ;; 2. The default signature file.
  (when-let* ((from (car (mail-header-parse-address (message-field-value "From"))))
              (domain (cadr (split-string from "@")))
              (message-signature-file
               (seq-find (lambda (name) (file-exists-p
                                    (concat message-signature-directory name)))
                         (list domain "default"))))
    (message-insert-signature)))

(defun pjones:mu4e-short-maildir (msg)
  "Format the maildir of MSG so it's as short as possible."
  (let* ((maildir (or (mu4e-message-field msg :maildir) ""))
         (prefix (substring maildir 1 2))
         (dir (file-name-nondirectory maildir)))
    (concat prefix "/" dir)))

(defun pjones:mu4e-org-capture ()
  "Capture the current email via `org-mode'."
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "m"))

(defun pjones:mu4e-common-binds (map)
  "Add common key bindings to MAP."
  (define-key map (kbd "C-c SPC") #'pjones:mu4e-org-capture))

(defun pjones:mu4e-headers-mode-hook ()
  "Hook function for `mu4e-headers-mode'."
  (pjones:mu4e-common-binds mu4e-headers-mode-map))

(defun pjones:mu4e-view-mode-hook ()
  "Hook function for `mu4e-view-mode'."
  (pjones:mu4e-common-binds mu4e-view-mode-map))

;; FIXME: Remove this after upgrading to 1.12.8:
(unless (boundp 'mu4e-trash-without-flag)
  (setf (plist-get (alist-get 'trash mu4e-marks) :action)
        (lambda (docid _msg target)
          (mu4e--server-move
           docid
           (mu4e--mark-check-target target) "-N")))) ; Instead of "+T-N")

;; General Settings:
(custom-set-variables
 '(mu4e-contexts (pjones:mu4e-contexts))
 '(mu4e-context-policy 'pick-first)
 '(mu4e-compose-context-policy 'ask)

 '(mu4e-attachment-dir "~/download/")
 '(mu4e-change-filenames-when-moving t)
 '(mu4e-completing-read-function 'completing-read)
 '(mu4e-date-format-long "%c")
 '(mu4e-get-mail-command "mbsync --all")
 '(mu4e-hide-index-messages t)
 '(mu4e-main-hide-personal-addresses t)
 '(mu4e-modeline-show-global nil)
 '(mu4e-trash-without-flag t) ; Move to trash without deleting.

 '(mu4e-search-include-related nil)
 '(mu4e-search-results-limit 500)
 '(mu4e-search-sort-direction 'ascending)
 '(mu4e-search-sort-field :date)
 '(mu4e-search-threads t)

 '(mu4e-headers-date-format "%a %F")
 '(mu4e-headers-long-date-format "%c")

 '(mu4e-view-scroll-to-next nil)

 '(mu4e-headers-fields
   '((:flags         . 4)
     (:human-date    . 14)
     (:from          . 15)
     (:thread-subject)))

 '(mu4e-maildir-shortcuts
   '(("/fastmail/Inbox" . ?i)
     ("/uni-tuebingen/Inbox" . ?I)))

 `(mu4e-bookmarks
   `((,(string-join
        '("flag:unread"
          "NOT m:/Trash/"
          "NOT m:/Archive/"
          "NOT m:/Spam/")
        " AND ") "Unread" ?u)
     ("flag:flagged" "Flagged messages" ?f)
     ("m:/Drafts/" "Drafts" ?d)
     ("m:/Sent/ d:today..now" "Sent today" ?s)
     ("m:/Archive/ d:1w..now" "Archived this week" ?a)
     ("m:/Spam/" "Spam" ?S)
     (,(string-join
        '("NOT m:/Archive/"
          "NOT m:/Trash/"
          "NOT m:/Sent/"
          "NOT m:/Spam/"
          "d:1w..now")
        " AND ") "Received this week" ?r))))

;; Extra Headers:
(add-to-list 'mu4e-header-info-custom
  '(:ua . (:name "User-Agent"
           :shortname "UA"
           :help "Mail User Agent"
           :function (lambda (msg)
                       (or (mu4e-message-field msg :user-agent) "")))))

(add-to-list 'mu4e-header-info-custom
  '(:short-maildir . (:name "Short Maildir"
                      :shortname "Dir"
                      :help "Shortened Maildir Name"
                      :function (lambda (msg)
                                  (pjones:mu4e-short-maildir msg)))))

(add-to-list 'mu4e-view-fields :ua t)

;; View Actions:
(add-to-list 'mu4e-view-actions '("open in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions '("tag message"     . mu4e-action-retag-message) t)

;; Hooks
(add-hook 'mu4e-compose-mode-hook #'mml-secure-message-sign)
(add-hook 'mu4e-compose-mode-hook #'pjones:mu4e-compose-mode-hook)
(add-hook 'mu4e-headers-mode-hook #'pjones:mu4e-headers-mode-hook)
(add-hook 'mu4e-view-mode-hook #'pjones:mu4e-view-mode-hook)

;;; mu4e-conf.el ends here
