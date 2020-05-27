;;; text-mode-conf.el -- Settings for with-editor
;;
;;; Commentary:
;;
;;; Code:
(require 'evil-leader)

(declare-function with-editor-finish "with-editor")
(declare-function with-editor-cancel "with-editor")
(defvar with-editor-mode)

(defun pjones:text-mode-finish ()
  "Finish whatever is being done in this buffer."
  (interactive)
  (cond
   (with-editor-mode
    (call-interactively 'with-editor-finish))))

(defun pjones:text-mode-cancel ()
  "Cancel whatever is being done in this buffer."
  (interactive)
  (cond
   (with-editor-mode
    (call-interactively 'with-editor-cancel))))

(evil-leader/set-key-for-mode 'text-mode
  "m c" #'pjones:text-mode-finish
  "m k" #'pjones:text-mode-cancel)

;;; text-mode-conf.el ends here
