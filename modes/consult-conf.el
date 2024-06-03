;;; consult-conf.el -- Settings for `consult' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'consult)
(require 'project)

(custom-set-variables
 '(consult-project-root-function #'pjones:consult-project-root-function))

(defvar pjones:consult-orig-buffer-items
  (plist-get consult--source-buffer :items)
  "Original function for fetching buffer names.")

(defun pjones:consult-source-buffer-items ()
  "Replacement function for `consult--source-buffer' :items.

This version of the :items function discards buffers that are
already being shown in a window."
  (seq-remove
   (lambda (name-and-buffer) (get-buffer-window (cdr name-and-buffer) 'visible))
   (funcall pjones:consult-orig-buffer-items)))

;; Update consult--source-buffer with our custom :items function:
(plist-put consult--source-buffer :items
           #'pjones:consult-source-buffer-items)

(defun pjones:consult-project-root-function ()
  "Return the current project's root directory."
  (when-let ((project (project-current)))
    (project-root project)))

;;; consult-conf.el ends here
