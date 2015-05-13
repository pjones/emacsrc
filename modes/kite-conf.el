;;; kite-conf.el -- Configuration for kite (Webkit debugger).
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'kite))

(setq kite-console-header ""
      kite-console-prompt "❯ "
      kite-console-prompt-internal
        (propertize kite-console-prompt 'font-lock-face 'kite-console-prompt-face))

;; This is here because I can't seem to get cl-flet to work and kite
;; isn't working correctly for me, it keeps raising errors when I
;; start the console.
(defun kite--default-error-handler (e)
  (message "Kite: %s" (plist-get e :message)))

(defun pjones:kite-console-mode-hook ()
  (define-key kite-console-mode-map (kbd "C-c C-l") 'kite-clear-console))

(add-hook 'kite-console-mode-hook 'pjones:kite-console-mode-hook)
