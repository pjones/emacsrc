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
 '(vterm-kill-buffer-on-exit nil) ; See pjones:vterm-mode-hook
 '(vterm-buffer-name-string "vterm %s"))

(let ((map vterm-mode-map))
  ;; Remove some bindings:
  (define-key map (kbd "M-'") nil)
  (define-key map (kbd "M-:") nil)

  ;; And add some new ones:
  (define-key map (kbd "C-c C-d") #'pjones:vterm-change-dir)
  (define-key map (kbd "C-c C-g") #'keyboard-quit)
  (define-key map (kbd "C-c C-M-r") #'pjones:vterm-toggle-name)
  (define-key map (kbd "C-c C-r") #'pjones:vterm-restore-cursor)
  (define-key map (kbd "C-c C-u") #'universal-argument)
  (define-key map (kbd "C-c C-x") #'vterm--self-insert)
  (define-key map (kbd "C-c M-x") #'vterm--self-insert)
  (define-key map (kbd "C-g") #'vterm--self-insert)
  (define-key map (kbd "C-u") #'vterm--self-insert)
  (define-key map (kbd "M-<backspace>") nil))

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
                      (project-prefixed-buffer-name "vterm")))))
  (vterm--set-title pjones:vterm-title)
  (force-mode-line-update))

(defun pjones:vterm-restore-cursor ()
  "Restore the `vterm' buffer cursor to the frame default."
  (interactive)
  (setq cursor-type (alist-get 'cursor-type default-frame-alist)))

(defun pjones:vterm-mode-hook ()
  "Mode hook for `vterm-mode'."
  (puni-mode -1) ; Disable puni mode.

  ;; If `vterm-kill-buffer-on-exit' is non-nil in the global scope
  ;; then very short lived processes will result in the frame being
  ;; deleted even if I want it to stay around.  That's because the
  ;; process may exit before I have a chance to set the variable to
  ;; `nil'.
  ;;
  ;; So for longer lived processes we can switch the value for the
  ;; current buffer to close the frame when the process exits.
  (unless (local-variable-p 'vterm-kill-buffer-on-exit)
    (setq-local vterm-kill-buffer-on-exit t)))

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

(defun pjones:vterm-frame ()
  "Start a new vterm instance."
  (interactive)
  (let ((buffer (vterm--internal #'identity t)))
    (pop-to-buffer
     buffer
     '((display-buffer-reuse-window
        display-buffer-pop-up-frame) .
        ((pop-up-frame-parameters . ((pjones-type . "vterm"))))))
    buffer))

(defun pjones:vterm-frame-cmd (cmd &optional keep)
  "Start a new vterm instance running CMD.
If KEEP is non-nil then don't kill the buffer when the command finishes."
  (let ((vterm-shell cmd)
        (vterm-buffer-name-string cmd)
        (vterm-kill-buffer-on-exit (not keep)))
    (with-current-buffer (pjones:vterm-frame)
      (setq-local vterm-kill-buffer-on-exit (not keep))
      (vterm--set-title cmd))))

(defun pjones:vterm-maybe-delete-frame (buffer _event)
  "Delete frame (or window) for BUFFER if certain conditions are met."
  (when-let* ((window (get-buffer-window buffer t))
              (frame (window-frame window))
              (others (length (delq window (window-list frame 'no-minibuf))))
              (type (or (frame-parameter frame 'pjones-type) "none"))
              vterm-kill-buffer-on-exit)
    (cond ((and (string= type "vterm")
                (= others 0))
           (delete-frame frame))
          ((> others 0)
           (delete-window window)))
    (kill-buffer buffer)))

(advice-add 'vterm--set-title :around #'pjones:vterm--set-title)
(add-hook 'vterm-copy-mode-hook #'pjones:vterm-copy-mode-hook)
(add-hook 'vterm-exit-functions #'pjones:vterm-maybe-delete-frame)
(add-hook 'vterm-mode-hook #'pjones:vterm-mode-hook)

;;; vterm-conf.el ends here
