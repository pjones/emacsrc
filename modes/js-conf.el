;;; js-conf.el -- Configuration options for js-mode (JavaScript).
;;; Commentary:
;;; Code:
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'kite)
  (require 'js))

;; JavaScript mode settings
(setq js-indent-level 2
      js-flat-functions t)

(defvar pjones:js-keywords
  '(("\\(function\\) *("
     (0 (progn (compose-region (match-beginning 1)
                               (match-end 1) "\u0192") nil))))
  "Extra keywords to add to JavaScript buffers.
The Unicode anonymous function code was stolen from
https://github.com/technomancy/emacs-starter-kit")

(defvar pjones:js-keywords-enabled nil
  "Internal variable used to keep track of
  `pjones:js-keywords`.")

(defun pjones:js-extra-keywords ()
  "Toggle adding some extra keywords to JavaScript buffers."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (if (not pjones:js-keywords-enabled)
        (font-lock-add-keywords 'js-mode pjones:js-keywords)
      (font-lock-remove-keywords 'js-mode pjones:js-keywords)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (caar pjones:js-keywords) nil t)
          (decompose-region (match-beginning 1) (match-end 1)))))
    (font-lock-fontify-buffer)
    (setq pjones:js-keywords-enabled (not pjones:js-keywords-enabled))
    (set-buffer-modified-p modified)))

(defun pjones:js-eval-with-kite (start end &optional show)
  "Send the JavaScript between START and END to kite."
  (let ((input (buffer-substring-no-properties start end)))
    (kite-console nil)
    (insert (if show input "/* Pasted code running... */"))
    (comint-send-input)
    (comint-add-to-input-history input)
    (kite-console-eval-input input)))

(defun pjones:js-eval-region-with-kite (show)
  "Send the current region to kite."
  (interactive "P")
  (pjones:js-eval-with-kite (region-beginning) (region-end) show))

(defun pjones:js-mode-hook ()
  "Configure JS mode and key bindings."
  (local-set-key (kbd "C-c C-k") 'pjones:js-extra-keywords)
  (local-set-key (kbd "C-c C-e") 'pjones:js-eval-region-with-kite))

(add-hook 'js-mode-hook 'pjones:js-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
