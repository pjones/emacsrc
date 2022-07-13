;;; magit-conf.el -- Customizations for magit.
;;
;;; Commentary:
;;
;;; Code:
(require 'forge)
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

;; Custom forges (GitLab, Enterprise GitHub, etc.):
(add-to-list 'forge-alist
             '("code.rfa.sc.gov"
               "code.rfa.sc.gov/api/v4"
               "code.rfa.sc.gov"
               forge-gitlab-repository))
;; Transient settings for magit:
(add-to-list 'transient-values '(magit-tag "--annotate" "--sign"))

(defun pjones:git-branch-prefix ()
  "Attempt to extract the current branch prefix."
  (pcase major-mode
    ('text-mode
     (save-excursion
       (goto-char (point-min))
       (when (search-forward-regexp "On branch \\([^-]+\\)")
         (match-string-no-properties 1))))))

(defun pjones:git-insert-branch-prefix ()
  "Insert the current branch prefix."
  (interactive)
  (insert (concat (pjones:git-branch-prefix) " | ")))

(define-key git-commit-mode-map (kbd "C-c C-b") #'pjones:git-insert-branch-prefix)

;;; magit-conf.el ends here
