;;; js-conf.el -- Configuration options for js-mode (JavaScript).
(eval-when-compile
  (require 'js)
  (require 'kite-console))

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

(defun pjones:js-send-code-to-kite (code)
  (kite-console nil)
  (kite-console-eval-input code))

(defun pjones:js-send-region-to-kite ()
  (interactive)
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (pjones:js-send-code-to-kite code)))

(defun pjones:js-send-buffer-to-kite ()
  (interactive)
  (let ((code (buffer-string)))
    (pjones:js-send-code-to-kite code)))

(defun pjones:js-mode-hook ()
  "Configure JS mode and key bindings."
  (local-set-key (kbd "C-c C-k")   'pjones:js-extra-keywords)
  (local-set-key (kbd "C-c C-r")   'pjones:js-send-region-to-kite)
  (local-set-key (kbd "C-c C-b")   'pjones:js-send-buffer-to-kite))
(add-hook 'js-mode-hook 'pjones:js-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
