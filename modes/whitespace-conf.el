;;; whitespace.el -- Configuration options for whitespace-mode.
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 'simple)
(require 'whitespace)

(defvar pjones:whitespace-style
  '(face tabs newline lines-tail trailing)
  "Default value for `whitespace-style'.")

(custom-set-variables
 '(whitespace-style pjones:whitespace-style)
 '(whitespace-action '(auto-cleanup)))

(defun pjones:whitespace-and-visual-line ()
  "Alter `whitespace-mode' when variable `visual-line-mode' is non-nil."
  (when whitespace-mode
    (setq-local whitespace-style
                (if visual-line-mode
                    (-reject
                     (apply-partially 'equal 'lines-tail)
                     pjones:whitespace-style)
                  pjones:whitespace-style))
    ;; I can't figure out how to force `whitespace-mode' to update
    ;; it's faces so I just toggle it.
    (whitespace-mode -1)
    (whitespace-mode +1)))

(add-hook 'visual-line-mode-hook #'pjones:whitespace-and-visual-line)

;;; whitespace-conf.el ends here
