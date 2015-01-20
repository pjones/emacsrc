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
(defun pmade-move-to-mail-group (folder)
  "Move the marked messages (or the current message) to the given
folder of the current group."
  (let* ((server (or (gnus-method-to-full-server-name
                      (gnus-find-method-for-group gnus-newsgroup-name))
                     "nnimap+mail.pmade.com"))
         (to-group (concat server ":" folder)))
    (gnus-summary-move-article nil to-group nil 'move)
    (message "Moved to group: %s" to-group)))

(defun pmade-gnus-summary-bottom ()
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

(defun pmade-summary-hook ()
  (define-key gnus-summary-mode-map
    (kbd "v a") (lambda ()
                  (interactive)
                  (pmade-move-to-mail-group "Archive")))

  (define-key gnus-summary-mode-map
    (kbd "v r") (lambda ()
                  (interactive)
                  (pmade-move-to-mail-group "Review")))

  (define-key gnus-summary-mode-map
    (kbd "v d") (lambda ()
                  (interactive)
                  (pmade-move-to-mail-group "Trash")))

  (define-key gnus-summary-mode-map
    (kbd "v f") 'gnus-summary-mail-forward)

  (define-key gnus-summary-mode-map
    (kbd "v l") (lambda ()
                  (interactive)
                  (pmade-move-to-mail-group "Rebekah")))

  (define-key gnus-summary-mode-map
    (kbd "v s") (lambda ()
                  (interactive)
                  (pmade-move-to-mail-group "Junk")))

  (local-set-key (vector 'remap 'end-of-buffer) 'pmade-gnus-summary-bottom))

(add-hook 'gnus-summary-mode-hook 'pmade-summary-hook)
