;;; macros.el -- Custom Lisp macros.
;;
;;; Commentary:
;;
;;; Code:
(defmacro pjones:jump-to-buffer (name &optional command)
  "Generate a command to jump to buffer NAME.
If buffer NAME doesn't exist, COMMAND can be used to create it."
  `(defun ,(intern (concat "pjones:jump-to-buffer-" name)) ()
    (interactive)
    (let ((buf (get-buffer ,name)))
      (if buf (display-buffer buf)
        ,(if command `(,command) 'nil)))))

(defmacro pjones:call-with-prefix-arg (func)
  "Generate a command to call FUNC with `C-u'."
  (let* ((funs (symbol-name (function func)))
         (name (intern (concat "pjones:" funs "-with-arg"))))
    `(defun ,name ()
       (interactive)
       (let ((current-prefix-arg '(4)))
         (call-interactively ,func)))))

;;; macros.el ends here
