;;; project-conf.el -- Settings for `project' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'project)
(require 'projectile)

;; Port this function to project.el:
(declare-function pjones:projectile-compile-project "./projectile-conf")

(defun pjones:project-shell-command nil
  "Run a shell command in the project's root directory."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shell-command)))

(defun pjones:project-async-shell-command nil
  "Run an asynchronous shell command in the project's root directory."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'async-shell-command)))

(let ((map project-prefix-map))
  (define-key map (kbd "!") #'pjones:project-shell-command)
  (define-key map (kbd "1") #'pjones:project-shell-command)
  (define-key map (kbd "&") #'pjones:project-async-shell-command)
  (define-key map (kbd "7") #'pjones:project-async-shell-command)
  (define-key map (kbd "c") #'pjones:projectile-compile-project)
  (define-key map (kbd "f") #'projectile-find-file)
  (define-key map (kbd "t") #'projectile-test-project))

;;; project-conf.el ends here
