;;; Settings that affect the summary view
(setq 
 gnus-extract-address-components 'mail-extract-address-components ; slower but more accurate address parsing
 gnus-auto-center-summary nil                                     ; don't auto-center the summary buffer
 gnus-list-identifiers "\\[[^]]*\\]")

;; How the summary line should look
(setq 
 pmade-sum-sep "%9{ ┆ %}"
 gnus-summary-mode-line-format "Gnus: %g"
 gnus-article-mode-line-format "Gnus: "
 gnus-summary-line-format (concat "%5{%U%R%z%}"   pmade-sum-sep "%6{%ud%}" pmade-sum-sep
                                  "%7{%-15,15a%}" pmade-sum-sep "%B" "%*" "%8{%s%}\n"))

;; When running inside a GUI version of Emacs, use unicode characters
;; for the tree view.
(when window-system
  (setq
   gnus-sum-thread-tree-indent          "  " 
   gnus-sum-thread-tree-root            "┌ " 
   gnus-sum-thread-tree-false-root      "- "
   gnus-sum-thread-tree-single-indent   "● "
   gnus-sum-thread-tree-leaf-with-other "├─> "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-single-leaf     "└─> "))

;; Functions for Key Bindings
(defun pmade-summary-move-to-archive ()
  "Move the current message to the archive group."
  (interactive)
  (gnus-summary-move-article nil pmade-archive-group nil 'move)
  (message (concat "Archived to " pmade-archive-group)))

(defun pmade-summary-mode-to-review ()
  "Move the current message to the review group."
  (interactive)
  (gnus-summary-move-article nil pmade-review-group nil 'move)
  (message (concat "Moved to review group: " pmade-review-group)))

(defun pmade-summary-hook ()
  (local-set-key "a" 'pmade-summary-move-to-archive)
  (local-set-key "r" 'pmade-summary-mode-to-review))

(add-hook 'gnus-summary-mode-hook 'pmade-summary-hook)
