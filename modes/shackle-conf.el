;;; shackle-conf.el -- Settings for shackle.

(custom-set-variables
 '(shackle-default-rule (quote (:select t)))
 '(shackle-rules nil))

;; Compilation buffers get a new frame:
(add-to-list 'shackle-rules '(compilation-mode :noselect t :frame t))

;; Man buffers should appear in the current window:
(add-to-list 'shackle-rules '("\\`\\*Man " :regexp t :same t))
