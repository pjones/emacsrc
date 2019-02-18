;;; undo-tree-conf.el -- Settings for undo-tree.
;;
;;; Commentary:
;;
;;; Code:
(require 'undo-tree)
(require 'diminish)

(defun pjones:undo-tree-mode-hook ()
  "Hook for `undo-tree-mode'."
  (diminish 'undo-tree-mode))

(add-hook 'undo-tree-mode-hook #'pjones:undo-tree-mode-hook)

;;; undo-tree-conf.el ends here
