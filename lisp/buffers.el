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

(setq display-buffer-alist
      `(;; Windows that should split the entire frame:
        (,(pjones:buffer-conditions
           '("\\*Backtrace\\*"
             "\\*Completions\\*"
             "\\*Deletions\\*"
             calendar-mode))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-at-bottom)
         (reusable-frames . t)
         (window-height . 0.3))

        ;; Like above, but with a smaller size:
        (,(pjones:buffer-conditions
           '("Embark Collect \\(Live\\|Completions\\)"))
         (display-buffer-at-bottom)
         (reusable-frames . t)
         (window-height . 0.1))

        ;; Buffers that are related to the current window and should
        ;; split it, opening a new window below the current window:
        (,(pjones:buffer-conditions
           '(grep-mode
             rg-mode
             "\\*magit-.*popup"
             "\\*Occur\\*"
             "\\*transient"))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-in-direction)
         (reusable-frames . t)
         (direction . below)
         (window-height . 0.4))

        ;; Buffers that should create a new frame:
        (,(pjones:buffer-conditions
           '(comint-mode
             compilation-mode
             haskell-interactive-mode
             inferior-python-mode
             shell-mode
             "\\*HTTP Response.*"))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-pop-up-frame)
         (inhibit-switch-frame . t)
         (reusable-frames . t)
         (pop-up-frame-parameters . ((unsplittable . t))))

        ;; Buffers that should take over the current window:
        (,(pjones:buffer-conditions
           '("\\*Org Agenda\\*"
             Man-mode))
         (display-buffer-same-window))))

;;; buffers.el ends here
