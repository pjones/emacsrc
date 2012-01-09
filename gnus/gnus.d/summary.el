;;; Settings that affect the summary view
(setq 
 gnus-extract-address-components 'mail-extract-address-components ; slower but more accurate address parsing
 gnus-auto-center-summary nil)                                    ; don't auto-center the summary buffer

;; Regular expressions to remove from the Subject line.  For example,
;; removing the name of a mailing list.
(setq gnus-list-identifiers 
  '("Re: \\[Medtronic (Aura Software)\\] *"
    "\\[LCML\\] *"
    "\\[\\w+-\\w+\\] *"))

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
(defun pmade-move-to-mail-group (group)
  "Move the marked messages (or the current message) to the given
IMAP mail group."
  (gnus-summary-move-article nil group nil 'move)
  (message (concat "Moved to group: " group)))

(defun pmade-summary-hook ()
  (local-set-key "a" (lambda () (interactive) (pmade-move-to-mail-group pmade-archive-group)))
  (local-set-key "l" (lambda () (interactive) (pmade-move-to-mail-group pmade-rebekah-group)))
  (local-set-key "r" (lambda () (interactive) (pmade-move-to-mail-group pmade-review-group)))
  (local-set-key "s" (lambda () (interactive) (pmade-move-to-mail-group pmade-spam-group))))

(add-hook 'gnus-summary-mode-hook 'pmade-summary-hook)
