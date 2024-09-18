;;; project-conf.el -- Settings for `project' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'project)

(declare-function magit-project-status "magit-extras")
(declare-function magit-file-dispatch "magit-files")
(declare-function vterm--internal "vterm")

(defun pjones:project-vterm ()
  "Start a `vterm' for the current project.
Ensures that the buffer name doesn't change so it can be found again."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer default-project-vterm-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer)
      (vterm--internal #'pop-to-buffer current-prefix-arg)
      ;; Keep my vterm code from overriding the new buffer name:
      (setq-local vterm-buffer-name-string nil)
      (rename-buffer (generate-new-buffer-name default-project-vterm-name)))))

(custom-set-variables
 '(project-switch-commands
   '((project-async-shell-command "Async" ?&)
     (magit-project-status "Magit" ?m)
     (pjones:project-vterm "Shell" ?s)
     (project-dired "Dired" ?d)
     (project-find-dir "Find Dir" ?D)
     (project-find-file "File" ?f)
     (project-shell-command "Run" ?!))))

(let ((map project-prefix-map))
  (define-key map (kbd "d") #'project-dired)
  (define-key map (kbd "D") #'project-find-dir)
  (define-key map (kbd "m") #'magit-project-status)
  (define-key map (kbd "M") #'magit-file-dispatch)
  (define-key map (kbd "s") #'pjones:project-vterm))

;;; project-conf.el ends here
