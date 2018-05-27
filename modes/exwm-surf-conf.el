;;; exwm-surf-conf.el --- Settings for exwm-surf.
;;
;;
;;; Commentary:
;;
;;
;;; Code:
(eval-when-compile
  (require 'exwm)
  (require 'exwm-surf))

(defvar pjones:surf-dir
  (expand-file-name "~/.surf/")
  "Directory where Surf stores its files.")

(custom-set-variables
 `(exwm-surf-history-file  ,(concat pjones:surf-dir "history"))
 `(exwm-surf-bookmark-file ,(concat pjones:surf-dir "bookmarks"))

 '(exwm-surf-search-prefixes-alist
   (quote (("g"  . "http://duckduckgo.com/?q=%s")
           ("go" . "http://www.google.com/search?q=%s")
           ("w"  . "http://en.wikipedia.org/w/index.php?title=Special:Search&search=%s&go=Go"))))

 '(exwm-surf-key-bindings
   (quote (("C-s"   . exwm-surf-search)
           ("C-r"   . exwm-surf-search)
           ("C-o"   . exwm-surf-history)
           ("C-l"   . exwm-surf-edit-url)
           ("M-C-w" . exwm-surf-url-to-kill-ring)
           ("M-C-y" . exwm-surf-yank-url)))))

(provide 'exwm-surf-conf)
;;; exwm-surf-conf.el ends here
