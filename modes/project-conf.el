;;; project-conf.el -- Settings for `project' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'project)

(declare-function magit-file-dispatch "magit-files")
(declare-function magit-project-status "magit-extras")
(declare-function pjones:vterm-frame "vterm-conf")

(defun pjones:project-vterm ()
  "Start a `vterm' for the current project.
Ensures that the buffer name doesn't change so it can be found again."
  (interactive)
  (require 'vterm)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer default-project-vterm-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (with-current-buffer (pjones:vterm-frame)
        ;; Keep my vterm code from overriding the new buffer name:
        (setq-local vterm-buffer-name-string nil)
        (rename-buffer (generate-new-buffer-name default-project-vterm-name))))))

(defun pjones:project-switch-project ()
  "Switch projects, probably in a new frame."
  (interactive)
  (if (project-current nil)
      (let ((switch-to-buffer-obey-display-actions t)
            (display-buffer-overriding-action '((display-buffer-pop-up-frame)) ))
        (call-interactively #'project-switch-project))
    (call-interactively #'project-switch-project)))

(defun pjones:project-compile (prompt)
  "Compile the current project.
When PROMPT is non-nil always prompt for the compile command."
  (interactive "P")
  (let* ((default-directory (project-root (project-current t)))
         (current-prefix-arg prompt))
    (if (and (not prompt) compile-command)
        (compile compile-command t)
      (call-interactively #'compile))))

(custom-set-variables
 '(project-switch-commands
   '((project-async-shell-command "Async" ?r)
     (magit-project-status "Magit" ?m)
     (pjones:project-vterm "Shell" ?s)
     (project-dired "Dired" ?d)
     (project-find-dir "Find Dir" ?D)
     (project-find-file "File" ?f)
     (project-shell-command "Run" ?!))))

(let ((map project-prefix-map))
  (keymap-set map "a" #'find-sibling-file)
  (keymap-set map "c" #'pjones:project-compile)
  (keymap-set map "d" #'project-dired)
  (keymap-set map "D" #'project-find-dir)
  (keymap-set map "m" #'magit-project-status)
  (keymap-set map "M" #'magit-file-dispatch)
  (keymap-set map "p" #'pjones:project-switch-project)
  (keymap-set map "P" #'project-switch-project)
  (keymap-set map "r" #'project-async-shell-command)
  (keymap-set map "R" #'project-query-replace-regexp)
  (keymap-set map "s" #'pjones:project-vterm))

;;; project-conf.el ends here
