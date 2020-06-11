;;; browse-url-conf.el -- Settings for browse-url.
;;
;;; Commentary:
;;
;;; Code:
;; Dependencies.
(require 'async)
(require 'browse-url)

(defun pjones:browse-url-browser-function (url &rest _args)
  "Open URL in a external browser."
  (interactive)
  (async-start-process "vimb" "vimb" nil url))

(setq browse-url-browser-function #'pjones:browse-url-browser-function)

;;; browse-url-conf.el ends here
