;;; buffers.el -- Control how buffers are displayed. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; I used to use the Shackle package (https://depp.brause.cc/shackle/)
;; but it was a bit unruly sometimes causing buffers to appear in the
;; minibuffer.
;;
;; This configuration is much simpler and uses the built-in
;; `display-buffer-alist' system.
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(defun pjones:buffer-name-or-mode-matches-p (name mode condition)
  "Return non-nil if CONDITION matches NAME or MODE.
If CONDITION is a string, treat it like a regular expression and
return non-nil if it matches NAME or MODE.  If CONDITION is a
symbol, compare it to MODE."
  (or (and (symbolp condition)
           (eq condition mode))
      (and (stringp condition)
           (or (string-match condition name)
               (string-match condition (symbol-name mode))))))

(defmacro pjones:buffer-conditions (names-or-modes)
  "Generate a condition function for `display-buffer-alist'.

NAMES-OR-MODES should be a list of regular expressions that match a
buffer name, or symbols that match a major mode."
  `(lambda (buffer-or-name _action)
     (when-let* ((buffer (and buffer-or-name (get-buffer buffer-or-name)))
                 (name (buffer-name buffer))
                 (mode (buffer-local-value 'major-mode buffer)))
       (-any
        (apply-partially #'pjones:buffer-name-or-mode-matches-p name mode)
        ,names-or-modes))))

(custom-set-variables
 ;; Don't hide frames, when deleting windows, just kill the frame:
 '(frame-auto-hide-function #'delete-frame)

 ;; Avoid switching to buffers already shown in windows:
 '(switch-to-prev-buffer-skip 'visible)

 ;; Always use the rules in this file:
 '(switch-to-buffer-obey-display-actions t)

 ;; Regular expressions that match buffers that should be skipped when
 ;; moving through the buffer list.
 '(switch-to-prev-buffer-skip-regexp
   '("\\*Help"
     "\\*Async-"
     "\\*envrc\\*"))

 ;; Default action if `display-buffer-alist' doesn't select an action:
 '(display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-pop-up-window
      display-buffer-reuse-mode-window
      display-buffer-use-some-window
      display-buffer-pop-up-frame) .
      ((reusable-frames . visible))))

 ;; Ensure that the current frame is used to display server buffers.
 ;; NOTE: This might not be necessary now that I removed an older
 ;; setting in the deleted file: server-conf.el.
 '(server-window
   (lambda (buffer)
     (let ((display-buffer-alist
            '((".*" .
               ((display-buffer-same-window) .
                nil)))))
       (pop-to-buffer buffer))))

 ;; Select a window for a buffer to be shown in:
 '(display-buffer-alist
   `(;; Buffers that should split the entire frame:
     (,(pjones:buffer-conditions
        '("\\*Backtrace\\*"
          "\\*Completions\\*"
          "\\*Deletions\\*"
          calendar-mode))
      (display-buffer-at-bottom)
      (window-height . 0.3))

     ;; Buffers that should take over the current window:
     (,(pjones:buffer-conditions
        '("\\*Org Agenda\\*"
          Man-mode
          magit-status-mode))
      (display-buffer-same-window)))))

;;; buffers.el ends here
