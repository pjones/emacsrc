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

(defun pmade-find-file-code-window nil
  "Open a file in the window I normally write code in."
  (interactive)
  (let* ((max (length (window-list)))
         (win (if (> max 3) 2 1))
         (buf (save-window-excursion
                (ido-find-file)
                (current-buffer))))
    (window-number-select win)
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
  (let* ((buf (get-buffer "*terminal*"))
         (max (- (length (window-number-list)) 1))
         (win (car (member buf (mapcar 'window-buffer (window-number-list))))))
    (if win (select-window (get-buffer-window win))
      (window-number-select max)
      (if buf (switch-to-buffer buf)
        (term "/opt/local/bin/zsh")))))
        
(defun pmade-select-window nil
  "Use ido completion to select a window based on its buffer name"
  (interactive)
  (let* ((bufs (mapcar 'window-buffer (window-list nil nil)))
         (sel (ido-completing-read "Window: " (mapcar 'buffer-name bufs)))
         (win (get-buffer-window (get-buffer sel))))
    (select-window win)))
    
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
  (let* ((win (window-number))
         (win-count (- (length (window-number-list)) 1))
         (main-win 2) (alt-win 1) main-buf alt-buf)
    (if (string= "skinny.local" system-name)
        (setq main-win 1 alt-win 2))
    (setq main-buf (current-buffer))
    (when (> win-count 1)
      (cond
       ((= win main-win)
        (setq main-buf (current-buffer)
              alt-buf (save-window-excursion
                        (window-number-select alt-win)
                        (current-buffer))))
       ((= win alt-win)
        (setq main-buf (current-buffer)
              alt-buf (save-window-excursion
                        (window-number-select main-win)
                        (current-buffer))))
       (t
        (setq alt-buf (current-buffer)
              main-buf (save-window-excursion
                         (window-number-select main-win)
                         (current-buffer))))))
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    (cond
     ;; Alt behavior
     (arg
      (split-window-horizontally 80)
      (setq main-win 2 alt-win 1))
     ;; On my laptop
     ((string= "skinny.local" system-name)
      (split-window-horizontally)
      (window-number-select 2)
      (split-window-vertically))
     ;; Default behavior
     (t
      (pmade-3-windows)
      (window-number-select 3)
      (split-window-vertically)))
    (setq win-count (- (length (window-number-list)) 1))
    (when (> win-count 2)
      (window-number-select win-count)
      (if (get-buffer "*terminal*")
          (switch-to-buffer "*terminal*")
        (term "/opt/local/bin/zsh")))
    (window-number-select alt-win)
    (if alt-buf (switch-to-buffer alt-buf))
    (window-number-select main-win)
    (switch-to-buffer main-buf)))
