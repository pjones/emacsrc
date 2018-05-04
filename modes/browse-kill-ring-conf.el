;;; browse-kill-ring-conf.el -- Settings for browse-kill-ring.

(custom-set-variables
 '(browse-kill-ring-display-style 'separated)
 '(browse-kill-ring-quit-action 'save-and-restore)
 `(browse-kill-ring-separator ,(make-string 70 ?─))
 '(browse-kill-ring-recenter nil) ; Setting to `t' causes a bug.
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item t)
 '(browse-kill-ring-display-duplicates nil))
