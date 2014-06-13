;;; browse-url-conf.el -- Settings for browse-url.
(eval-when-compile
  (require 'gnus-msg)
  (require 'message)
  (require 'browse-url))

(declare-function message-goto-body "message")
(declare-function message-goto-to "message")
(declare-function message-goto-subject "message")
(declare-function gnus-group-mail "gnus-msg")

(defun pjones:new-mail-in-gnus (url)
  (let ((to (error "not implemented"))
        (subject (error "not implemented")))
    (gnus-group-mail)
    (message-goto-to)
    (insert to)
    (unless (null subject)
      (message-goto-subject)
      (insert subject))
    (message-goto-body)))

(setq browse-url-generic-program "open-url"
      browse-url-browser-function 'browse-url-generic
      browse-url-mailto-function  'pjones:new-mail-in-gnus)
