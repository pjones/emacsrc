;;; vterm-conf.el -- Settings for `vterm' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'vterm)

(declare-function project-prefixed-buffer-name "project")
(declare-function project-root "project")
(declare-function puni-mode "puni")

(defvar pjones:vterm-title nil
  "The last title set in the current buffer.")

(custom-set-variables
  '(vterm-buffer-name-string "vterm %s"))

(let ((map vterm-mode-map))
  (define-key map (kbd "M-'") nil)
  (define-key map (kbd "C-c C-d") #'pjones:vterm-change-dir)
  (define-key map (kbd "C-c C-r") #'pjones:vterm-restore-cursor)
  (define-key map (kbd "C-c C-M-r") #'pjones:vterm-toggle-name)
  (define-key map (kbd "C-c C-x") #'vterm--self-insert)
  (define-key map (kbd "C-c M-x") #'vterm--self-insert))

(defun pjones:vterm-change-dir (dir)
  "Change to DIR in the current vterm shell."
  (interactive (list (read-directory-name "cd: ")))
  (vterm-insert "cd " dir)
  (vterm-send-return))

(defun pjones:vterm-toggle-name ()
  "Toggle buffer name between project and title."
  (interactive)
  (if (local-variable-p 'vterm-buffer-name-string)
      (kill-local-variable 'vterm-buffer-name-string)
    (setq-local vterm-buffer-name-string nil)
    (let* ((project (project-current nil))
           (default-directory (if project
                                  (project-root project)
                                default-directory)))
      (rename-buffer (generate-new-buffer-name
                      (project-prefixed-buffer-name "shell")))))
  (vterm--set-title pjones:vterm-title)
  (force-mode-line-update))

(defun pjones:vterm-restore-cursor ()
  "Restore the `vterm' buffer cursor to the frame default."
  (interactive)
  (setq cursor-type (alist-get 'cursor-type default-frame-alist)))

(defun pjones:vterm-mode-hook ()
  "Mode hook for `vterm-mode'."
  (puni-mode -1)) ; Disable puni mode.

(defun pjones:vterm-copy-mode-hook ()
  "Mode hook for `vterm-copy-mode'."
  (pjones:vterm-restore-cursor)
  ;; Don't move beyond prompt, breaks copy mode:
  (setq-local next-line-add-newlines nil))

(defun pjones:vterm--set-title (orig title)
  "Wrapper around `vterm--set-title'.
ORIG is the original version of `vterm--set-title' and TITLE is the
new title to use."
  (pjones:vterm-restore-cursor)
  (setq-local pjones:vterm-title title)
  (if vterm-buffer-name-string
      (progn
        (funcall orig title)
        (setq mode-name "VTerm"))
    (setq mode-name (concat "VTerm " title))))

(advice-add 'vterm--set-title :around #'pjones:vterm--set-title)
(add-hook 'vterm-mode-hook #'pjones:vterm-mode-hook)
(add-hook 'vterm-copy-mode-hook #'pjones:vterm-copy-mode-hook)

;;; vterm-conf.el ends here
