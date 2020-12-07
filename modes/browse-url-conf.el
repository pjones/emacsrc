;;; browse-url-conf.el -- Settings for browse-url.  -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
;; Dependencies.

(require 'async)
(require 'browse-url)
(require 's)

(defun pjones:browse-url-browser-function (url &rest _args)
  "Open URL in a external browser."
  (interactive)
  (let ((prog
         (if (s-prefix-p "file:" url)
             "browser-sidebar"
           "browser-main")))
    (async-start-process prog prog nil url)))

(setq browse-url-browser-function #'pjones:browse-url-browser-function)

;;; browse-url-conf.el ends here
