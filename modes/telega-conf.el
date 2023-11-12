;;; telega-conf.el -- Settings for `telega' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'telega)
(require 'flyspell)

(custom-set-variables
 '(telega-chat-markup-functions '("markdown2" "org" nil)))

(unless (fboundp 'telega-flyspell-input-p)
  (defun telega-flyspell-input-p ()
    "Return non-nil if point is inside chatbuf's input."
    (> (point) telega-chatbuf--input-marker)))

(defun pjones:telega-chat-mode-hook ()
  "Hook for `telega-chat-mode'."
  (when (fboundp 'telega-flyspell-input-p)
    (setq flyspell-generic-check-word-predicate #'telega-flyspell-input-p)
    (flyspell-mode))
  (visual-line-mode))

(add-hook 'telega-chat-mode-hook #'pjones:telega-chat-mode-hook)

;;; telega-conf.el ends here
