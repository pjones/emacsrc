;;; js-conf.el -- Configuration options for js-mode (JavaScript).
(eval-when-compile (require 'js))

;; JavaScript mode settings
(setq js-indent-level 2
      js-flat-functions t)

(defvar pjones:js-keywords
  '(("\\(function *\\)("
     (0 (progn (compose-region (match-beginning 1)
                               (match-end 1) "\u0192") nil))))
  "Extra keywords to add to JavaScript buffers.
The Unicode anonymous function code was stolen from
https://github.com/technomancy/emacs-starter-kit")

(defun pjones:js-add-extra-keywords ()
  "Add some extra keywords to JavaScript buffers."
  (interactive)
  (font-lock-add-keywords 'js-mode pjones:js-keywords)
  (font-lock-fontify-buffer))

(defun pjones:js-rm-extra-keywords ()
  "Remove some extra keywords from JavaScript buffers."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (font-lock-remove-keywords 'js-mode pjones:js-keywords)
    (remove-text-properties (point-min) (point-max) '(composition nil))
    (set-buffer-modified-p modified)))

(defun pjones:js-mode-hook ()
  "Configure JS mode and key bindings."
  (local-set-key (kbd "C-c C-k")   'pjones:js-rm-extra-keywords)
  (local-set-key (kbd "C-c C-S-k") 'pjones:js-add-extra-keywords))
(add-hook 'js-mode-hook 'pjones:js-mode-hook)

;; Set up the cool extra keywords right away!
(pjones:js-add-extra-keywords)
