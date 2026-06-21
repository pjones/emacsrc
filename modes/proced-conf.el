;;; proced-conf.el -- Settings for `proced' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'proced)

(defun pjones:proced-process-basename (attrs)
  "Replace the args attribute with the base-name of a process.
ATTRS is the process attributes given by `proced'."
  (let ((args (cdr (assq 'args attrs))))
    (if (not (string-prefix-p "/" args))
        (cons 'args args)
      (let ((fields (split-string args)))
        (cons 'args
              (string-join
               (append (list (file-name-nondirectory (car fields)))
                       (drop 1 fields))
               " "))))))

(custom-set-variables
 '(proced-auto-update-flag 5)
 '(proced-enable-color-flag t)
 '(proced-custom-attributes (list #'pjones:proced-process-basename)))

;; Kill the buffer on quit.  I don't want the buffer updating in the
;; background.
(define-key proced-mode-map [remap quit-window] #'kill-current-buffer)

;;; proced-conf.el ends here
