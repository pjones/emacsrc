;;; Miscellaneous Functions
(defun open-line-below-like-vim ()
  "Open a line below the point, and move there"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above-like-vim ()
  "Open a line above the point, and move there"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun switch-to-previous-buffer ()
  "Switch back to the previous buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun kill-region-or-backward-kill-word (arg)
  "Replacement for `kill-region'.  If there is a region with
`transient-mark-mode' active, it will be removed and placed in
the kill ring in a similar manner to `kill-region'.  If there
isn't a region, the word before point will be deleted (without
placing it in the kill ring)."
  (interactive "p")
  (if (or (not transient-mark-mode) (and transient-mark-mode mark-active))
      (kill-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word (- arg)) (point)))))

(defun save-to-kill-ring-and-normalize-whitespace ()
  (interactive)
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (kill-new
     (replace-regexp-in-string "^\s+" "" (replace-regexp-in-string "\n\s*" " " text))))
  (deactivate-mark))

;; Based on smart tab: http://www.emacswiki.org/cgi-bin/emacs-en/TabCompletion
(defvar pmade-inside-smart-tab nil)

(defun pmade-smart-org-cycle ()
  "Prevent recursion with org-mode"
  (if (not pmade-inside-smart-tab)
      (let ((pmade-inside-smart-tab t)) (org-cycle))
    (indent-for-tab-command)))

(defun pmade-smart-tab ()
  "Context based tab key.  If there is a region, indent the
  region.  Otherwise attempt to perform tab completion or
  indentation."
  (interactive)
  (cond
   ((minibufferp) (minibuffer-complete))
   ((string= major-mode "org-mode") (pmade-smart-org-cycle))
   (mark-active (indent-region (region-beginning) (region-end)))
   ((looking-at "\\_>") (hippie-expand nil))
   (t (indent-for-tab-command))))

;; Help start ERC
(defun pmade-erc-start (&optional bitlbee-only)
  "Load and start ERC.  With prefix key, only connect to bitlbee."
  (interactive "P")
  (load "~/.emacs.d/pmade/pmade-erc")
  (erc :server "127.0.0.1")
  (unless bitlbee-only (erc :server "irc.freenode.net")))

(defun pmade-3-windows ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun rebekah-journal ()
  (interactive)
  (find-file "~/Documents/people/rebekah/journal/2009.org")
  (goto-char (point-max)))

(defun pmade-transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
