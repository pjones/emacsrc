;;; magit-conf.el -- Customizations for magit.
;;
;;; Commentary:
;;
;;; Code:
(require 'git-rebase)
(require 'magit)
(require 'with-editor)

(defun pjones:magit-repository-directories ()
  "Generate a directory list for `magit-list-repositories'."
  (let ((base (expand-file-name "~/src"))
        (ls (lambda (dir) (directory-files dir t "^[^.]"))))
    (-map (lambda (dir) (cons dir 0))
          (-filter
           (lambda (dir) (file-directory-p (concat dir "/.git/")))
           (-flatten (-map ls (funcall ls base)))))))

(custom-set-variables
 '(magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
 '(magit-popup-use-prefix-argument 'default)
 '(magit-status-margin '(t age magit-log-margin-width nil 18))
 '(magit-status-show-hashes-in-headers t)
 '(magit-section-initial-visibility-alist
   '(([unpushed status] . show)))
 '(magit-repository-directories
   (pjones:magit-repository-directories)))


;;; magit-conf.el ends here
