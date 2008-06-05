;; Options and Hooks for Integrating with Mutt
;; Some of this was ripped off from mutt-mode:
;; http://www.drao-ofr.hia-iha.nrc-cnrc.gc.ca/~rreid/software/mutt/mutt.el

(defvar mail-signature-dir "~/etc/signatures"
  "The directory that contains files that are mail signatures")

;; Function that is called when we enter mail-mode
(defun pmade-mail-mode-hook ()
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ; kill quoted sigs
  (set-buffer-modified-p nil)                       ; pretend we didn't modify the buffer
  (setq make-backup-files nil)                      ; no backup files
  (setq version-control nil)                        ; no backup versions
  (turn-on-auto-fill)                               ; enable auto-filling paragraphs
  (flyspell-mode t)                                 ; enable spell checking
  (rename-buffer "*Composing*" t)                   ; make the buffer name look better
  (cd "~")                                          ; get out of /tmp
  (bind-keys-for-mail-mode)                         ; bind a few keys
  (mail-text)                                       ; move to the body
  (and (looking-at "^radiant@pmade.com") (reply-to-form-mail)))

;; Bind special keys in mail mode
(defun bind-keys-for-mail-mode ()
  (local-set-key [?\C-c ?\C-c] 'exit-emacs-and-return-to-mutt)
  (local-set-key [?\C-c ?\C-s] 'mail-signature-next)
  (local-set-key [?\C-c ?h ?t] 'headers-goto-to)
  (local-set-key [?\C-c ?h ?c] 'headers-goto-cc)
  (local-set-key [?\C-c ?h ?f] 'headers-goto-fcc)
  (local-set-key [?\C-c ?h ?k] 'headers-goto-keywords)
  (local-set-key [?\C-c ?h ?s] 'headers-goto-subject)
  (local-set-key [?\C-c ?h ?b] 'headers-goto-bcc)
  (local-set-key [?\C-c ?h ?r] 'headers-goto-reply-to)
  (local-set-key [?\C-c ?h ?f] 'headers-goto-from)
  (local-set-key [?\C-c ?h ?o] 'headers-goto-organization)
  (local-set-key [?\C-c ?h ?a] 'headers-attach-file)
  (local-set-key [?\C-c ?g ?b] 'mail-text)
  (local-set-key [?\C-c ?g ?s] 'mail-goto-signature)
  (local-set-key [?\C-c ?k ?s] 'kill-to-signature)
  (local-set-key [?\C-c ?k ?c] 'mail-delete-old-citations))

;; Functions that will get bound to keys
(defun exit-emacs-and-return-to-mutt ()
  "Save the current buffer and exit emacs, returning to mutt"
  (interactive)
  (save-buffers-kill-emacs t))

(defun mail-delete-old-citations ()
  "Delete citations more than one level deep from buffer."
  (interactive)
  (goto-char (point-min))
  (flush-lines "^[ \t\f]*>[ \t\f]*>[ \t\f>]*"))

(defun mail-goto-signature ()
  "Go to the beginning of the message signature."
  (interactive)
  (goto-char (point-max))
  (and 
   (save-match-data (re-search-backward "^-- \n" nil t))
   (previous-line 1)))

(defun kill-to-signature ()
  "Kill from point to just before the signature"
  (interactive)
  (let ((beg (point)))
    (mail-goto-signature)
    (delete-region beg (point))))
  
(defun headers-position-on-value ()
  (beginning-of-line)
  (skip-chars-forward "-A-Za-z0-9:")
  ;; XXX - Should make sure we stay on line.
  (forward-char))

(defun headers-goto-field (field)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (save-match-data
      (when (re-search-forward (concat "^\\($\\|" field ": \\)"))
	(if (looking-at "^$")
	    (progn
	      (insert-string field ": \n")
	      (forward-char -1))
	  (headers-position-on-value))))))

(defmacro define-header-goto (name header)
  `(defun ,name ()
     ,(concat "Position the cursor on the " header ": header.")
     (interactive)
     (headers-goto-field ,header)))

(define-header-goto headers-goto-to "To")
(define-header-goto headers-goto-cc "Cc")
(define-header-goto headers-goto-fcc "Fcc")
(define-header-goto headers-goto-keywords "Keywords")
(define-header-goto headers-goto-subject "Subject")
(define-header-goto headers-goto-bcc "Bcc")
(define-header-goto headers-goto-reply-to "Reply-To")
(define-header-goto headers-goto-from "From")
(define-header-goto headers-goto-organization "Organization")

(defun reply-to-form-mail ()
  "Reply to badly formatted form mail"
  (interactive)
  (mail-text)
  ;; Update the from address, taken from the body
  (let ((name) (email))
    (save-match-data
      (when (re-search-forward "Sender Name:")
        (skip-chars-forward " ")
        (let ((beg (point)))
          (move-end-of-line 1)
          (setq name (buffer-substring beg (point)))))
      (when (re-search-forward "Sender E-mail:")
        (skip-chars-forward " ")
        (let ((beg (point)))
          (move-end-of-line 1)
          (setq email (buffer-substring beg (point))))))
    (headers-goto-to)
    (kill-line)
    (insert (concat name " <" email ">")))
  ;; Kill the attribution line
  (mail-text)
  (kill-line 2)
  ;; Clear the subject line
  (headers-goto-subject)
  (save-match-data (and (re-search-forward "Re: ") (kill-line))))
      
(defun mail-signature-next ()
  "Replace the current signature with the next one"
  (interactive)
  (or (boundp 'mail-signature-files)
      (setq mail-signature-files
            (directory-files (expand-file-name mail-signature-dir) t "^[A-Za-z0-9]")))
  (let ((sig-file (first mail-signature-files)))
    (setq mail-signature-files (append (cdr mail-signature-files) (list (car mail-signature-files))))
    (save-excursion
      (mail-goto-signature)
      (next-line 2)
      (let ((beg (point)))
        (end-of-buffer)
        (kill-region beg (point)))
      (insert-file-contents sig-file))))
    
  
   
;; Register Mutt files as mail files, and start mail-mode
(add-to-list 'auto-mode-alist '("/tmp/mutt.*" . mail-mode))
(add-hook 'mail-mode-hook 'pmade-mail-mode-hook)