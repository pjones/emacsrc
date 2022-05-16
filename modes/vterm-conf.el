;;; vterm-conf.el -- Settings for `vterm' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'vterm)

(declare-function puni-mode "puni")

(custom-set-variables
  '(vterm-buffer-name-string "vterm %s"))

(let ((map vterm-mode-map))
  (define-key map (kbd "C-c C-d") #'pjones:vterm-change-dir)
  (define-key map (kbd "C-c C-g") #'vterm-send-C-g)
  (define-key map (kbd "C-c C-x") #'vterm-send-C-x))

(defun pjones:vterm-change-dir (dir)
  "Change to DIR in the current vterm shell."
  (interactive (list (read-directory-name "cd: ")))
  (vterm-insert "cd " dir)
  (vterm-send-return))

(defun pjones:vterm-mode-hook ()
  "Mode hook for `vterm-mode'."
  (puni-mode -1)) ; Disable puni mode.

(defun pjones:vterm-copy-mode-hook ()
  "Mode hook for `vterm-copy-mode'."
  ;; Restore cursor if it was forced to block:
  (setq cursor-type (alist-get 'cursor-type default-frame-alist))
  ;; Don't move beyond prompt, breaks copy mode:
  (setq-local next-line-add-newlines nil))

(add-hook 'vterm-mode-hook #'pjones:vterm-mode-hook)
(add-hook 'vterm-copy-mode-hook #'pjones:vterm-copy-mode-hook)

;;; vterm-conf.el ends here
