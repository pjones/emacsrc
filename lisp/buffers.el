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

(require 'dash)

(defmacro pjones:buffer-conditions (names-or-modes)
  "Generate a condition function for `display-buffer-alist'.

NAMES-OR-MODES should be a list of regular expressions that match a
buffer name, or symbols that match a major mode."
  `(lambda (buffer-or-name _action)
     (when-let* ((buffer (and buffer-or-name (get-buffer buffer-or-name)))
                 (name (buffer-name buffer))
                 (mode (buffer-local-value 'major-mode buffer)))
       (-any
        (lambda (condition)
          (or (and (symbolp condition)
                   (eq condition mode))
              (and (stringp condition)
                   (string-match condition name))))
        ,names-or-modes))))

(defun pjones:display-buffer-maybe-pop-up-frame (buffer alist)
  "Pop up a frame, unless we're in a special frame already.
BUFFER and ALIST are passed on to display functions."
  (let* ((frame (selected-frame))
         (name (frame-parameter frame 'role)))
    (if (string= name "notes")
        (display-buffer-in-direction buffer alist)
      (display-buffer-pop-up-frame buffer alist))))

(setq display-buffer-alist
      `(;; Windows that should split the entire frame:
        (,(pjones:buffer-conditions
           '("\\*Backtrace\\*"
             "\\*Completions\\*"
             "\\*Deletions\\*"
             "\\*evil-owl\\*"
             "\\*eww buffers\\*"
             "\\*JS scratch\\*"
             "\\*magit-.*popup"
             "\\*transient"
             help-mode
             pdf-outline-buffer-mode
             calendar-mode))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-at-bottom)
         (reusable-framaes .)
         (window-height . 0.3))

        ;; Like above, but with a smaller size:
        (,(pjones:buffer-conditions
           '("Embark Collect \\(Live\\|Completions\\)"))
         (display-buffer-at-bottom)
         (reusable-framaes .)
         (window-height . 0.1))

        ;; Windows that should split the current window but *not* get
        ;; focus:
        (,(pjones:buffer-conditions
           '("magit-diff: "
             "\\*HTTP Response.*"))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-at-bottom)
         (reusable-framaes .)
         (window-height . 0.4))

        ;; Buffers that should take over the current window:
        (,(pjones:buffer-conditions
           '("\\*Org Agenda\\*"))
         (display-buffer-same-window))

        ;; Modes to show in the right side-window:
        (,(pjones:buffer-conditions
           '("\\*eldoc\\*"
             "\\*flymake message\\*"))
         (display-buffer-in-side-window)
         (side . bottom))

        ;; Modes that force a new (raised and focused) frame:
        (,(pjones:buffer-conditions
           '(term-mode
             haskell-interactive-mode))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-pop-up-frame)
         (reusable-framaes .)
         (dedicated . t)
         (pop-up-frame-parameters
          . ((name . "popup") ; For the window manager.
             (x-name . "popup" ) ; Because `name' is replaced with `title'.
             (unsplittable . t))))

        ;; Modes that share a frame that is never raised:
        (,(pjones:buffer-conditions
           '(compilation-mode
             grep-mode
             rg-mode))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          pjones:display-buffer-maybe-pop-up-frame)
         (direction . right)
         (reusable-frames . t)
         (dedicated . t)
         (inhibit-switch-frame . t)
         (pop-up-frame-parameters
          . ((name . "popup") ; For the window manager.
             (x-name . "popup" ) ; Because `name' is replaced with `title'.
             (unsplittable . t))))))

;;; buffers.el ends here
