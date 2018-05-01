;;; browse-url-conf.el -- Settings for browse-url.
(eval-when-compile
  (require 'browse-url))

(defun pjones:browse-url (url &optional _new-window)
  "Open a URL in a external browser."
  (interactive)
  (call-process "bspc" nil 0 nil "desktop" "browsers" "--focus")
  (call-process "chromium" nil 0 nil url))

(setq browse-url-browser-function #'pjones:browse-url)
