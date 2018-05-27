;;; browse-url-conf.el -- Settings for browse-url.
(eval-when-compile
  (require 'browse-url))

;; Dependencies.
(require 'async)

(defun pjones:browse-url (url &optional _new-window)
  "Open a URL in a external browser."
  (interactive)
  (async-start-process "surf" "surf" nil url))

(setq browse-url-browser-function #'pjones:browse-url)
