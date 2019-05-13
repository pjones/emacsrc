;;; captain-conf.el -- Customize captain mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'captain)

(defun pjones:captain-text-mode ()
  "Always capitalize words."
  (setq captain-predicate
        (lambda () t)))

(defun pjones:captain-prog-mode ()
  "Only capitalize in source comments."
  (setq captain-predicate
        (lambda ()
          (nth 8 (syntax-ppss (point))))))

(defun pjones:captain-org-mode ()
  "Don't capitalize in org source blocks."
  (setq captain-predicate
        (lambda ()
          (not (org-in-src-block-p)))))

(defun pjones:captain-mode-hook ()
  "Mode hook for `captain-mode'."
  (define-abbrev captain-mode-abbrev-table "i" "I" nil :count 1))

(add-hook 'captain-mode-hook #'pjones:captain-mode-hook)
(add-hook 'text-mode-hook    #'pjones:captain-text-mode)
(add-hook 'prog-mode-hook    #'pjones:captain-prog-mode)
(add-hook 'org-mode-hook     #'pjones:captain-org-mode)

;;; captain-conf.el ends here
