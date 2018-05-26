;;; elscreen-conf.el -- Settings for elscreen.el
(eval-when-compile
  (require 'elscreen))

(defun pjones:elscreen-create (clone)
  "Create and name a new elscreen.

When CLONE is non-nil clone the current screen."
  (interactive "P")
  (if clone (elscreen-clone) (elscreen-create))
  (call-interactively 'elscreen-screen-nickname))

(custom-set-variables
 `(elscreen-prefix-key ,(kbd "s-z"))
 '(elscreen-default-buffer-initial-message nil)
 '(elscreen-display-tab nil)
 '(elscreen-tab-display-control nil))

;; elscreen loads too late so we need to help it boot:
(elscreen-make-frame-confs (selected-frame))
