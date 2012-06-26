;;; functions.el -- Non-interactive functions
(eval-when-compile
  (require 'org)
  (require 'org-clock))

(defun pjones:org-clock-time ()
  "Returns a formatted org clock time if currently clocked in."
  (if (and (fboundp 'org-clocking-p) (org-clocking-p))
      (substring-no-properties (org-clock-get-clock-string)) ""))
(defalias 'pmade:org-clock-time 'pjones:org-clock-time)

(defun pjones:urgency-hint (frame status)
  (let* ((wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints (if status (logior flags #x00000100) (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))
