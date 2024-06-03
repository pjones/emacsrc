;;; functions.el -- Non-interactive functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

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

(defun pjones:prog-mode-list ()
  "Return a list of mode names that derive from `prog-mode'."
  ;; Plus some stupid modes that don't seem to show up:
  (append '("ruby" "python")
          (mapcar (lambda (mode)
                    (string-replace "-mode" "" (symbol-name mode)))
                  (seq-filter (lambda (mode)
                                (and (symbolp mode)
                                     (provided-mode-derived-p mode 'prog-mode)))
                              (mapcar 'cdr auto-mode-alist)))))

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
