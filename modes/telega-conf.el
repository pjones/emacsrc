;;; telega-conf.el -- Settings for `telega' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'telega)
(require 'jinx)

(custom-set-variables
 '(telega-chat-input-markups '("markdown2" "org" nil)))

(defun pjones:telega-chat-mode-hook ()
  "Hook for `telega-chat-mode'."
  (jinx-mode)
  (visual-line-mode))

(add-hook 'telega-chat-mode-hook #'pjones:telega-chat-mode-hook)

;;; telega-conf.el ends here
