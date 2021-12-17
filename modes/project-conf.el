;;; project-conf.el -- Settings for `project' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'project)
(require 'projectile)

;; Port this function to project.el:
(declare-function pjones:projectile-compile-project "./projectile-conf")

(let ((map project-prefix-map))
  (define-key map (kbd "c") #'pjones:projectile-compile-project)
  (define-key map (kbd "d") #'project-dired)
  (define-key map (kbd "D") #'project-find-dir)
  (define-key map (kbd "f") #'projectile-find-file)
  (define-key map (kbd "r") #'projectile-run-project)
  (define-key map (kbd "s") #'projectile-run-vterm)
  (define-key map (kbd "t") #'projectile-test-project))

;;; project-conf.el ends here
