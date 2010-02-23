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

(defun pmade-find-file-window-2 ()
  "Open a file in window number 2."
  (interactive)
  (let ((buf (save-window-excursion
               (ido-find-file)
               (current-buffer))))
    (window-number-select 2)
    (switch-to-buffer buf)))

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
  (find-file "~/Documents/people/rebekah/journal/2010.org")
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

(defun pmade-goto-terminal nil
  "Jump to the screen and window with the terminal"
  (interactive)
  (escreen-goto-screen-3)
  (dolist (window (window-number-list))
    (when (string= "*terminal*" (buffer-name (window-buffer window)))
      (select-window window))))

(defun pmade-split-frame (&optional arg)
  "Split the frame based on the current escreen"
  (interactive "P")
  (let ((screen (escreen-get-current-screen-number)))
    (cond 
     ((= screen 1) (pmade-split-frame:screen-1 arg))
     ((= screen 2) (pmade-split-frame:screen-2 arg))
     ((= screen 3) (pmade-split-frame:screen-3 arg)))))

(defun pmade-split-frame:screen-1 (&optional arg)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (pmade-3-windows)
  (window-number-select 1)
  (split-window-vertically)
  (switch-to-buffer "#emacs")
  (window-number-select 2)
  (switch-to-buffer "#photogeeks")
  (window-number-select 3)
  (split-window-vertically)
  (switch-to-buffer "&bitlbee")
  (window-number-select 4)
  (switch-to-buffer "*scratch*")
  (window-number-select 5)
  (split-window-vertically)
  (switch-to-buffer "#latex")
  (window-number-select 6)
  (switch-to-buffer "#sproutcore")
  (window-number-select 3))

(defun pmade-split-frame:screen-2 (&optional arg)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (pmade-3-windows)
  (window-number-select 2)
  (if (get-buffer "*Org Agenda*")
      (switch-to-buffer "*Org Agenda*")
    (org-agenda nil "d"))
  (window-number-select 1)
  (switch-to-buffer "review.org")
  (window-number-select 3)
  (switch-to-buffer "sevendesign.org")
  (window-number-select 2))

(defun pmade-split-frame:screen-3 (&optional arg)
  (let ((win (window-number))
        (buf2 (and (> 1 (length (window-number-list)))
                   (save-window-excursion
                     (window-number-select 2)
                     (current-buffer))))
        main-buf alt-buf)
    (cond
     ((= win 2)
      (setq main-buf (current-buffer))
      (setq alt-buf (save-window-excursion 
                      (window-number-select 1)
                      (current-buffer))))
     ((= win 1)
      (setq main-buf (current-buffer))
      (setq alt-buf buf2))
     (t
      (setq main-buf buf2)
      (setq alt-buf (current-buffer))))
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    (cond
     (arg ;; Alt behavior
      (split-window-horizontally 80)
      (if alt-buf (switch-to-buffer alt-buf))
      (window-number-select 2)
      (switch-to-buffer main-buf))
     (t ;; Default behavior
      (pmade-3-windows)
      (window-number-select 3)
      (split-window-vertically)
      (window-number-select 4)
      (if (get-buffer "*terminal*")
          (switch-to-buffer "*terminal*")
        (term "/opt/local/bin/zsh"))
      (window-number-select 1)
      (if alt-buf (switch-to-buffer alt-buf))
      (window-number-select 2)
      (switch-to-buffer main-buf)))))
