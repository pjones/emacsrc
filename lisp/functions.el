;;; functions.el -- Non-interactive functions
;;; Commentary:
;;; Code:
(defun pjones:urgency-hint (frame status)
  (let* ((wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints (if status (logior flags #x00000100) (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t))
  (x-send-client-message ; This is for Wayland EWHM changes.
     frame 0 frame "_NET_WM_STATE" 32
     '(1 "_NET_WM_STATE_DEMANDS_ATTENTION" 0))
  frame)

(defvar pjones:cheat-sheet-buffers nil
  "List of cheat sheet buffers.")

(defun pjones:cheat-sheet-buffer (name contents)
  "Create a buffer NAME with CONTENTS.
Always returns the buffer's name."
  (let ((exists (get-buffer name)))
    (unless exists
      (save-window-excursion
        (with-current-buffer (get-buffer-create name)
          (insert contents)
          (goto-char (point-min))
          (markdown-mode)
          (set-buffer-modified-p nil)
          (add-to-list 'pjones:cheat-sheet-buffers name))))
    name))

(defun pjones:kill-cheat-sheet-buffers ()
  "Delete cheat sheet buffers."
  (dolist (name pjones:cheat-sheet-buffers)
    (let ((buf (get-buffer name)))
      (when buf (kill-buffer-if-not-modified buf)))))

(provide 'functions)
;;; functions.el ends here
