;;; shackle-conf.el -- Settings for shackle.
(eval-when-compile
  (require 'shackle))

(custom-set-variables
 '(shackle-default-rule (quote (:select t)))
 '(shackle-rules nil))

;; Compilation buffers get a new frame:
(add-to-list 'shackle-rules '(compilation-mode :noselect t))

;; Man buffers should appear in the current window:
(add-to-list 'shackle-rules '("\\`\\*Man " :regexp t :same t))

;; When asking for completions:
(add-to-list 'shackle-rules '("*Completions*" :size 0.3 :align t))

;; Don't select grep buffers:
(add-to-list 'shackle-rules '(grep-mode :noselect t))

;; Rules for Magit buffers:
(add-to-list 'shackle-rules '(magit-status-mode :same t))
(add-to-list 'shackle-rules '(magit-diff-mode :noselect t))

;; Circe buffers shouldn't split the frame:
(add-to-list 'shackle-rules '(:custom pjones:circe-windows :same t))

(defun pjones:circe-windows (buffer _alist _plist)
  "Return non-nil if BUFFER is a Circe buffer."
  (with-current-buffer buffer
    (string-match "^circe-" (symbol-name major-mode))))
