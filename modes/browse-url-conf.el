;;; browse-url-conf.el -- Settings for browse-url.
;;
;;; Commentary:
;;
;;; Code:
;; Dependencies.
(require 'async)
(require 'browse-url)

(defun pjones:browse-url-browser-function (url &optional new-window &rest args)
  "Open URL in a external browser.
If NEW-WINDOW is non-nil then always open a new browsers window.
Other arguments in ARGS are ignored."
  (interactive)
  (async-start-process "firefox" "firefox" nil "--new-window" url))

(setq browse-url-browser-function #'pjones:browse-url-browser-function)

;;; browse-url-conf.el ends here
