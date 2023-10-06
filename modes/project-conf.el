;;; project-conf.el -- Settings for `project' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'project)

(declare-function magit-project-status "magit-extras")
(declare-function magit-file-dispatch "magit-files")
(declare-function vterm "vterm")

(defun pjones:project-vterm ()
  "Start a `vterm' for the current project.
Ensures that the buffer name doesn't change so it can be found again."
  (interactive)
    (cl-letf (((symbol-function 'shell)
               (lambda (&optional arg)
                 (vterm arg)
                 (setq-local vterm-buffer-name-string nil)
                 (rename-buffer arg))))
      (call-interactively #'project-shell)))

(custom-set-variables
 '(project-switch-commands
   '((project-async-shell-command "Async" ?&)
     (magit-project-status "Magit" ?m)
     (pjones:project-vterm "Shell" ?s)
     (project-dired "Dired" ?d)
     (project-find-file "File" ?f)
     (project-shell-command "Run" ?!))))

(let ((map project-prefix-map))
  (define-key map (kbd "d") #'project-dired)
  (define-key map (kbd "D") #'project-find-dir)
  (define-key map (kbd "m") #'magit-project-status)
  (define-key map (kbd "M") #'magit-file-dispatch)
  (define-key map (kbd "s") #'pjones:project-vterm))

;;; project-conf.el ends here
