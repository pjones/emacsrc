;;; mastodon-conf.el -- Settings for `mastodon' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'mastodon)

(declare-function visual-fill-mode "visual-fill")

;; https://codeberg.org/martianh/mastodon.el
(custom-set-variables
 '(mastodon-auth-use-auth-source nil)
 '(mastodon-instance-url "https://hostux.social/")
 '(mastodon-active-user "devalot"))

(defun pjones:mastodon-toot-mode-hook ()
  "Hook for `mastodon-toot-mode'."
  (auto-fill-mode -1)
  (visual-line-mode))

(add-hook 'mastodon-toot-mode-hook #'pjones:mastodon-toot-mode-hook)

;;; mastodon-conf.el ends here
