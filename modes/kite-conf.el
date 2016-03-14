;;; kite-conf.el -- WebKit inspector front-end
;;; Commentary:
;;; Code:
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'warnings)
  (require 'kite))

;; Kite throws too many errors.
(defun kite--default-error-handler (error-result)
  (message "Kite: %s" (plist-get error-result :message)))

(custom-set-variables
 '(kite-console-header "")
 '(kite-console-prompt "❯ ")
 '(kite-console-on-reload-function (lambda nil nil)))

;; Kite Bug: This should be a function:
(setq kite-console-prompt-internal
      (propertize kite-console-prompt
                  'font-lock-face
                  'kite-console-prompt-face))

(defun pjones:kite-console-reset ()
  "Reload a page and reset the console."
  (interactive)
  ;; One of the functions below moves point beyond the buffer limit
  ;; which results in a beginning-of-buffer error.  This is why I
  ;; ignore the 'websocket error type in warning-suppress-types below.
  (when kite-most-recent-session
    (kite-reload-page t)
    (kite-clear-console)))

(defun pjones:kite-console-mode-hook ()
  "Mode hook for kite-console."
  (setq websocket-callback-debug-on-error nil)
  (set (make-local-variable 'warning-suppress-types)
       '((websocket)))
  (let ((map kite-console-mode-map))
    (define-key map (kbd "C-c C-l") 'kite-clear-console)
    (define-key map (kbd "C-c C-r") 'pjones:kite-console-reset)))

(add-hook 'kite-console-mode-hook 'pjones:kite-console-mode-hook)

(provide 'kite-conf)
;;; kite-conf.el ends here

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
