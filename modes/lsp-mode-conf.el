;;; lsp-mode-conf.el -- Settings for `lsp-mode'
;;
;;; Commentary:
;;
;;; Code:
(require 'lsp-mode)

(custom-set-variables
 '(lsp-keep-workspace-alive nil)
 '(lsp-modeline-code-actions-enable nil)
 '(lsp-ui-sideline-enable t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top))

(custom-set-faces
 '(lsp-ui-doc-background ((t (:inherit fringe))))
 '(lsp-ui-doc-header ((t (:inherit mode-line)))))

(defun pjones:update-lsp-ui-doc-border ()
  "Update the border color for LSP doc frames."
  (setq lsp-ui-doc-border (face-background 'mode-line nil t)))

(add-hook 'pjones:after-theme-change-hook #'pjones:update-lsp-ui-doc-border)
(add-hook 'lsp-ui-doc-mode-hook #'pjones:update-lsp-ui-doc-border)

;;; lsp-mode-conf.el ends here
