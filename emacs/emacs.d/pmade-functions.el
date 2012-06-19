;;; Miscellaneous Functions
(defun pmade-maybe-save-buffers-kill-terminal (&optional arg)
  "Save me from myself.  I somehow keep hitting C-x C-c when I
don't want to."
  (interactive)
  (if (yes-or-no-p "Really close this terminal? ")
      (save-buffers-kill-terminal arg)))

(defun pmade-open-line-above ()
  "Open a line above point and move there."
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-according-to-mode))

(defun pmade-open-line-below ()
  "Open a line below the point, and move there"
  (interactive)
  (move-end-of-line 1)
  (newline)
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

(defun pmade-irc (&optional local-only)
  "Start IRC client.  With an argument only start a connection to
the local bitlbee instance."
  (interactive "P")
  (load "pmade-rcirc")
  (if local-only (rcirc-connect "localhost")
    (rcirc nil)
    (when (= 1 (length (window-list)))
      (split-window-right)
      (split-window-below)
      (split-window-below)
      (windmove-right)
      (split-window-below)
      (split-window-below)
      (balance-windows))))

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

(defun pmade-select-window nil
  "Use ido completion to select a window based on its buffer name"
  (interactive)
  (let* ((bufs (mapcar 'window-buffer (window-list nil nil)))
         (sel (ido-completing-read "Window: " (mapcar 'buffer-name bufs)))
         (win (get-buffer-window (get-buffer sel))))
    (select-window win)))

(defun pmade-toggle-dictionary ()
  (interactive)
  (let ((dict ispell-current-dictionary))
    (ispell-change-dictionary
     (if (string= dict "italian") "english" "italian"))
    (activate-input-method
     (if (string= dict "italian") nil "italian-postfix"))))

(defun pmade-insert-italian-name ()
  "Helper function for when I'm writing Italian dialog."
  (interactive)
  (let* ((names '("Alessandra" "Piera" "Carlotta" "Stefano"
                  "Valentina" "Paolo"))
         (pick (ido-completing-read "Name: " names)))
    (insert (concat "*" pick ":* "))))

(defun pmade:org-clock-time ()
  "Returns a formatted org clock time if currently clocked in."
  (if (and (fboundp 'org-clocking-p) (org-clocking-p))
      (substring-no-properties (org-clock-get-clock-string)) ""))

(defun pmade:urgency-hint (frame status)
  (let* ((wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints (if status (logior flags #x00000100) (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))
