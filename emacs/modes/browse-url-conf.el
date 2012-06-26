;;; browse-url-conf.el -- Settings for browse-url.
(eval-when-compile (require 'browse-url))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open-url")
