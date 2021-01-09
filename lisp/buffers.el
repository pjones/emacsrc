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

(require 'dash)

(defmacro pjones:buffer-conditions (names-or-modes)
  "Generate a condition function for `display-buffer-alist'.

NAMES-OR-MODES should be a list of regular expressions that match a
buffer name, or symbols that match a major mode."
  `(lambda (buffer-or-name _action)
     (when buffer-or-name
       (let* ((buffer (get-buffer buffer-or-name))
              (name (buffer-name buffer))
              (mode (buffer-local-value 'major-mode buffer)))
         (-any
          (lambda (condition)
            (or (and (symbolp condition)
                     (eq condition mode))
                (and (stringp condition)
                     (string-match condition name)))
            ) ,names-or-modes)))))

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
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-at-bottom)
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

        ;; Modes that share a frame that is never raised.
        (,(pjones:buffer-conditions
           '("\\*eldoc\\*"
             "\\*flymake "
             compilation-mode
             grep-mode
             rg-mode))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-pop-up-frame)
         (reusable-frames . t)
         (dedicated . t)
         (inhibit-switch-frame . t)
         (pop-up-frame-parameters
          . ((name . "popup") ; For the window manager.
             (x-name . "popup" ) ; Because `name' is replaced with `title'.
             (unsplittable . t))))))

;;; buffers.el ends here
