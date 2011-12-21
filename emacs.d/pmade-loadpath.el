;; Places I keep elisp files
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/pmade")
(add-to-list 'load-path "~/.emacs.d/packages")

;; Local site-lisp
(setq pmade-site-lisp
      (cond
       ((file-exists-p "~/.emacs.d/share/emacs/site-lisp") "~/.emacs.d/share/emacs/site-lisp")
       ((file-exists-p "/usr/local/share/emacs/site-lisp") "/usr/local/share/emacs/site-lisp")))

(add-to-list 'load-path pmade-site-lisp)
(add-to-list 'load-path (concat pmade-site-lisp "/gnus/lisp"))
(add-to-list 'load-path (concat pmade-site-lisp "/rinari"))
(add-to-list 'load-path (concat pmade-site-lisp "/haskell"))
(add-to-list 'load-path (concat pmade-site-lisp "/idea"))
