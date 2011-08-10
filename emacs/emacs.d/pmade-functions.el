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
         (win (if (> max 2) 2 1))
         (buf (save-window-excursion
                (ido-find-file)
                (current-buffer))))
    (idea-select-window win)
    (switch-to-buffer buf)))

(defun pmade-find-file-window-1 ()
  (interactive)
  (let ((new-buf
         (save-window-excursion
           (ido-find-file)
           (current-buffer))))
    (idea-select-window 1)
    (switch-to-buffer new-buf)))

(defun pmade-pwgen (&optional kill-only)
  "Generate and insert a password."
  (interactive "P")
  (let ((pw (replace-regexp-in-string "\n" ""
              (shell-command-to-string "pwgen -cnB 12 1"))))
    (kill-new pw)
    (if (not kill-only) (insert pw))))

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

(defun pmade-goto-terminal ()
  "Jump to the terminal"
  (interactive)
  (shell-command "osascript -e 'tell application \"Terminal\" to activate'"))
        
(defun pmade-select-window nil
  "Use ido completion to select a window based on its buffer name"
  (interactive)
  (let* ((bufs (mapcar 'window-buffer (window-list nil nil)))
         (sel (ido-completing-read "Window: " (mapcar 'buffer-name bufs)))
         (win (get-buffer-window (get-buffer sel))))
    (select-window win)))
    
(defun pmade-make-calc-frame ()
  (interactive)
  (let* ((old (selected-frame))
         (new (make-frame)))
    (select-frame new)
    (set-frame-size new 147 35)
    (set-frame-position new 1920 1098)
    (scroll-bar-mode -1)
    (full-calc)
    (select-frame old)))

(defun pmade-toggle-dictionary ()
  (interactive)
  (let ((dict ispell-current-dictionary))
    (ispell-change-dictionary
     (if (string= dict "italian") "english" "italian"))
    (activate-input-method
     (if (string= dict "italian") nil "italian-postfix"))))

(defun pmade-schedule ()
  "Load the daily schedule into a buffer."
  (interactive)
  (let* ((file "~/Documents/pmade/pmade-inc/planning/schedule/schedule.yml")
         (rawnames (shell-command-to-string (concat "schedule.rb --names " file)))
         (names (split-string rawnames nil t))
         (pick (ido-completing-read "Schedule: " names))
         (buf (get-buffer-create "*schedule*")))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (shell-command-to-string (concat "schedule.rb --orgout " file " " pick)))
        (org-mode)
        (hl-line-mode t)
        (goto-char (point-min))
        (show-all)))
    (switch-to-buffer buf)))
