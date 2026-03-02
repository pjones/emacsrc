;;; link-hint-conf.el -- Settings for `link-hint' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'link-hint)

(defun avy-action-link-eww (pt)
  "Open link at PT in eww."
  (goto-char pt)
  (let* ((link-hint-types (link-hint--valid-types :copy))
         (link (link-hint--get-link-at-point)))
    (when link
      (eww (plist-get link :args)))))

;; link-hint hides these variables:
(defvar link-hint-avy-all-windows)
(setq link-hint-avy-all-windows t)

(add-to-list 'link-hint-dispatch-alist
             '(?e . avy-action-link-eww))

;;; link-hint-conf.el ends here
