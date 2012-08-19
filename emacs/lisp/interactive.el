;;; interactive.el -- Interactive functions.
(eval-when-compile
  (require 'cl)                         ; for plusp (need to replace it)
  (require 'ispell))

;; Hush some compiler warnings.
(declare-function pjones:rcirc-windows "../modes/rcirc-conf.el")

(defun pjones:maybe-save-buffers-kill-terminal (&optional arg)
  "Save me from myself.  I somehow keep hitting C-x C-c when I
don't want to."
  (interactive)
  (if (yes-or-no-p "Really close this terminal? ")
      (save-buffers-kill-terminal arg)))

(defun pjones:kill-region-or-backward-kill-word (arg)
  "Replacement for `kill-region'.  If there is a region with
`transient-mark-mode' active, it will be removed and placed in
the kill ring in a similar manner to `kill-region'.  If there
isn't a region, the word before point will be deleted (without
placing it in the kill ring)."
  (interactive "p")
  (if (or (not transient-mark-mode) (and transient-mark-mode mark-active))
      (kill-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word (- arg)) (point)))))

(defun pjones:switch-to-previous-buffer ()
  "Switch back to the last buffer shown in this window."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun pjones:open-line-above ()
  "Open a line above point and move there."
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-according-to-mode))

(defun pjones:open-line-below ()
  "Open a line below the point, and move there"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun pjones:irc (&optional local-only)
  "Start IRC client.  With an argument only start a connection to
the local bitlbee instance."
  (interactive "P")
  (require 'rcirc) ; loads in my rcirc-conf.el file
  (if local-only (rcirc-connect "localhost")
    (rcirc nil))
  (pjones:rcirc-windows))

(defun pjones:pwgen (&optional kill-only)
  "Generate and insert a password."
  (interactive "P")
  (let ((pw (replace-regexp-in-string "\n" ""
              (shell-command-to-string "pwgen -cnB 12 1"))))
    (kill-new pw)
    (if (not kill-only) (insert pw))))

(defun pjones:transpose-windows (arg)
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

(defun pjones:toggle-dictionary ()
  (interactive)
  (let ((dict ispell-current-dictionary))
    (ispell-change-dictionary
     (if (string= dict "italian") "english" "italian"))
    (activate-input-method
     (if (string= dict "italian") nil "italian-postfix"))))

(defun pjones:insert-italian-name ()
  "Helper function for when I'm writing Italian dialog."
  (interactive)
  (let* ((names '("Alessandra" "Piera" "Carlotta" "Stefano"
                  "Valentina" "Paolo"))
         (pick (ido-completing-read "Name: " names)))
    (insert (concat "*" pick ":* "))))

(defun pjones:journal ()
  "Open an entry in the journal for today."
  (interactive)
  (let* ((base (expand-file-name "~/documents/journal/"))
         (path (downcase (format-time-string "%Y/%m/%d-%A.md")))
         (full (concat base path))
         (dir (file-name-directory full)))
    (unless (file-directory-p dir) (make-directory dir t))
    (find-file full)
    (when (= (buffer-size) 0)
      (insert (format-time-string "%% %A, %B %d, %Y\n\n")))))

(defun pjones:bookmark (&optional set)
  "Quicker access to Emacs bookmarks.  Prompts for a bookmark and
then jumps to that bookmark.  If a prefix argument is given, set
a bookmark instead."
  (interactive "P")
  (if set (call-interactively 'bookmark-set)
    (call-interactively 'bookmark-jump)))

(defun pjones:inc-file ()
  "Given that the current file name is a number, increment that
number and open a file with the incremented number as a name."
  (interactive)
  (let* ((base (file-name-nondirectory buffer-file-name))
         (dir  (file-name-directory buffer-file-name))
         (name (file-name-sans-extension base))
         (ext  (file-name-extension base t))
         (num  (string-to-number name))
         (fmt  (concat "%0" (number-to-string (length name)) "d"))
         (new  (format fmt (1+ num))))
    (find-file (concat dir new ext))))

(defun pjones:window-config (&optional set)
  "Simple shortcut for remembering a window configuration and
then jumping back to it later.  With a prefix argument, save the
current window configuration.  Otherwise restore the window
configuration."
  (interactive "P")
  (if set (window-configuration-to-register ?.)
    (jump-to-register ?.)))

(defun pjones:kill-file-name (&optional full-path)
  "Create a new kill containing the base name of the buffer's
file.  With a prefix argument kill the entire path for the file."
  (interactive "P")
  (let* ((path (buffer-file-name))
         (name (file-name-nondirectory path)))
    (kill-new (if full-path path name))))
