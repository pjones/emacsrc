;;; functions.el -- Non-interactive functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(defun pjones:urgency-hint (frame status)
  "Set the urgency hint on FRAME with STATUS."
  (let* ((wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints (if status (logior flags #x00000100) (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t))
  (x-send-client-message ; This is for Wayland EWHM changes.
     frame 0 frame "_NET_WM_STATE" 32
     '(1 "_NET_WM_STATE_DEMANDS_ATTENTION" 0))
  frame)

(defun pjones:frame-popup-p (&optional frame)
  "Return non-nil if FRAME is a popup frame."
  (let ((params (frame-parameters (or frame (selected-frame)))))
    ;; See buffers.el for info about `x-name'.
    (or (string= "popup" (cdr (assq 'x-name params)))
        (string= "popup" (cdr (assq 'name params))))))

(defun pjones:display-buffer-in-non-popup-frame (buffer)
  "Display and select BUFFER for a server client."
  (let* ((not-popup-p (lambda (frame) (not (pjones:frame-popup-p frame))))
         (actions (list :frame-predicate not-popup-p)))
    (if-let ((window (display-buffer-use-some-frame buffer actions)))
        (progn
          (select-frame-set-input-focus (window-frame window))
          (select-window window))
      ;; Fallback.
      (pop-to-buffer buffer))))

(defun pjones:script (name)
  "Generate an absolute path to the script NAME."
  (concat
   (file-name-directory
    (directory-file-name
     (file-name-directory
      (or load-file-name
          byte-compile-current-file
          (buffer-file-name)))))
   "scripts/" name))

;;; functions.el ends here
