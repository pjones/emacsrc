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

(defmacro pjones:selected-buffer-conditions (names-or-modes &optional exceptions)
  "Generate a condition function for `display-buffer-alist'.

NAMES-OR-MODES should be a list of regular expressions that match
a buffer name, or symbols that match a major mode.  They will be
compared against the currently selected buffer, not the one being
displayed.

However, if the buffer to be shown matches a name or mode in
EXCEPTIONS then return nil to indicate that we do want that
buffer displayed in this frame and to let another rule control
its display."
  `(lambda (buffer-or-name action)
     (unless (funcall (pjones:buffer-conditions ,exceptions)
                      buffer-or-name action)
       (when-let* ((window (selected-window))
                   (buffer (window-buffer window)))
         (funcall
          (pjones:buffer-conditions ,names-or-modes)
          buffer action)))))

(defvar pjones:modes-dedicated-to-frames
  '(comint-mode
    compilation-mode
    grep-mode
    haskell-interactive-mode
    rg-mode
    shell-mode)
  "Modes that are displayed in their own frame.

When displaying these buffers, pop open a new frame.  When a
different buffer is being displayed, try to find a different
frame for it.")

(defvar pjones:dedicated-frame-exceptions
  '(" \\*transient\\*")
  "Names of buffers or modes that can be shown in dedicated frames.")

(custom-set-variables
 ;; Don't hide frames, when deleting windows, just kill the frame:
 '(frame-auto-hide-function #'delete-frame)

 ;; Regular expressions that match buffers that should be skipped when
 ;; moving through the buffer list.
 '(switch-to-prev-buffer-skip-regexp
   '("\\*Help"
     "\\*Async-"
     "\\*envrc\\*"))

 ;; Default action if `display-buffer-alist' doesn't select an action:
 '(display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-pop-up-window)) . nil)

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
   `(;; Buffers that must not be displayed in the current frame:
     (,(pjones:selected-buffer-conditions
        pjones:modes-dedicated-to-frames
        pjones:dedicated-frame-exceptions)
      (display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-use-some-frame
       display-buffer-pop-up-frame)
      (reusable-frames . visible))

     ;; Buffers that should pop out into a new frame:
     (,(pjones:buffer-conditions
        pjones:modes-dedicated-to-frames)
      (display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-pop-up-frame)
      (inhibit-switch-frame . t)
      (reusable-frames . visible)
      (pop-up-frame-parameters
       . ((unsplittable . t)
          (no-focus-on-map . t)
          (name . "popup"))))

     ;; Buffers that should split the entire frame:
     (,(pjones:buffer-conditions
        '("\\*Backtrace\\*"
          "\\*Completions\\*"
          "\\*Deletions\\*"
          calendar-mode))
      (display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-at-bottom)
      (window-height . 0.3))

     ;; Like above, but with a smaller size:
     (,(pjones:buffer-conditions
        '("Embark Collect \\(Live\\|Completions\\)"))
      (display-buffer-at-bottom)
      (window-height . 0.1))

     ;; Buffers that are related to the current window and should
     ;; split it, opening a new window below the current window:
     (,(pjones:buffer-conditions
        '("\\*HTTP Response.*"
          "\\*magit-.*popup"
          "\\*Occur\\*"
          "\\*transient"
          help-mode
          pdf-outline-buffer-mode))
      (display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-in-direction)
      (direction . below)
      (window-height . 0.4))

     ;; Buffers that should take over the current window:
     (,(pjones:buffer-conditions
        '("\\*Org Agenda\\*"
          Man-mode
          magit-status-mode))
      (display-buffer-same-window)))))

;;; buffers.el ends here
