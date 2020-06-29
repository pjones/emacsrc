;;; eglot-conf.el -- Settings for eglot
;;
;;; Commentary:
;;
;;; Code:
(require 'eglot)

(custom-set-variables
 '(eglot-auto-display-help-buffer nil)
 '(eglot-autoshutdown t))

(defvar pjones:eglot-doc-cleanup
  (list
   (list (rx (and "```haskell" ?\n (1+ (not (any ?:))) ?\n ":: ")
             (group (1+ (not (any ?`)))) ?\n "```") "`\\1`")
   (list (rx (1+ ?* (1+ (in space)))) "")
   (list (rx (and bol ?* ?\n)) ""))
  "List of regular expressions and replacement strings.
Used for cleaning up LSP server docs.")

(defun pjones:eglot-apply-doc-cleanup (string)
  "Clean STRING according to `pjones:eglot-doc-cleanup'."
  (let ((result string))
    (dolist (pat pjones:eglot-doc-cleanup)
      (while (string-match (car pat) result)
          (setq result (replace-match (cadr pat) nil nil result))))
    result))

(defun pjones:eglot--format-markup-advice (orig markup)
  "Clean docs from naughty LSP servers.
Call ORIG after modifying MARKUP"
  (let ((clean (pjones:eglot-apply-doc-cleanup (plist-get markup :value))))
    (apply orig (list (plist-put markup :value clean)))))

;; Before converting markdown documentation from the LSP server, clean
;; it up a bit.
(advice-add
 'eglot--format-markup
 :around #'pjones:eglot--format-markup-advice)

;;; eglot-conf.el ends here
