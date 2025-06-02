;;; meow-conf.el -- Settings for `meow' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'meow)
(require 'meow-tree-sitter)
(require 'transient)

(declare-function avy-goto-char-timer "avy")
(declare-function pjones:sort-lines "../list/interactive")
(declare-function puni-bounds-of-list-around-point "puni")
(declare-function puni-bounds-of-sexp-around-point "puni")
(declare-function puni-expand-region "puni")
(defvar avy-all-windows)
(defvar puni-mode)

(defun pjones:meow-sort (n)
  "Sort the region or N lines."
  (interactive "p")
  (unless (use-region-p)
    (meow-line n))
  (let ((beg (mark t))
        (end (point)))
    (pjones:sort-lines beg end)
    ;; FIXME: this isn't working:
    (thread-first
      (meow--make-selection '(expand . line) beg end t)
      (meow--select-without-history))))

(defun pjones:meow-jump (no-selection)
  "Move via `avy-goto-char-timer'.
When NO-SELECTION is non-nil, don't activate a selection."
  (interactive "P")
  (let ((avy-all-windows nil)
        (beg (point))
        end)
    (save-mark-and-excursion
      (avy-goto-char-timer)
      (setq end (point)))
    (if no-selection
        (goto-char end)
      (thread-first
        (meow--make-selection '(expand . word) beg end)
        (meow--select)))))

(defun pjones:meow-uncancel-selection ()
  "Restore the last selection."
  (interactive)
  (activate-mark))

(defun pjones:meow-block ()
  "Select or expand the current sexp."
  (interactive)
  (if puni-mode
      (let (beg end)
        (save-mark-and-excursion
          (puni-expand-region)
          (setq beg (region-beginning)
                end (region-end)))
        (thread-first
          (meow--make-selection '(expand . block) beg end)
          (meow--select)))
    (meow-block nil)))

(defun pjones:meow-quit ()
  "Close a buffer or window."
  (interactive)
  (let* ((map (intern (concat (symbol-name major-mode) "-map")))
         (cmd (keymap-lookup (symbol-value map) "q")))
    (if cmd (call-interactively cmd)
      (call-interactively #'meow-quit))))

;; When there is no selection, have:
;;
;;   - `meow-pop-selection' reactivate the mark
;;
;;   - `meow-kill' delete the character under the cursor
;;
(setopt meow-selection-command-fallback
        (append '((meow-pop-selection . pjones:meow-uncancel-selection)
                  (meow-kill . meow-delete))
                (assq-delete-all 'meow-pop-selection
                                 meow-selection-command-fallback)))

;; Override some mode's default state:
(dolist (entry '((mu4e-main-mode . insert)
                 (mu4e-view-mode . motion)
                 (vterm-mode     . insert)))
  (add-to-list 'meow-mode-state-list entry))

(meow-motion-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))

(meow-leader-define-key
 ;; Use SPC (0-9) for digit arguments.
 '("-" . negative-argument)
 '("/" . meow-keypad-describe-key)
 '("0" . meow-digit-argument)
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument))

(meow-normal-define-key
 '("'" . repeat)
 '("," . meow-inner-of-thing)
 '("-" . negative-argument)
 '("." . meow-bounds-of-thing)
 '("0" . meow-expand)
 '("1" . meow-expand)
 '("2" . meow-expand)
 '("3" . meow-expand)
 '("4" . meow-expand)
 '("5" . meow-expand)
 '("6" . meow-expand)
 '("7" . meow-expand)
 '("8" . meow-expand)
 '("9" . meow-expand)
 '(";" . meow-reverse)
 '("<escape>" . ignore)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 '("a" . meow-append)
 '("B" . meow-back-symbol)
 '("b" . meow-back-word)
 '("c" . meow-change)
 '("d" . pjones:meow-block)
 '("DEL" . pjones:meow-jump)
 '("F" . meow-next-symbol)
 '("f" . meow-next-word)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . isearch-forward-thing-at-point)
 '("o" . meow-open-above)
 '("O" . meow-open-below)
 '("q" . pjones:meow-quit)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("RET" . meow-line)
 '("S" . meow-mark-symbol)
 '("s" . meow-mark-word)
 '("S-<return>" . meow-goto-line)
 '("t" . "C-c t")
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("w" . meow-save)
 '("W" . meow-sync-grab)
 '("X" . meow-backward-delete)
 '("x" . meow-kill)
 '("Y" . consult-yank-pop)
 '("y" . meow-yank)
 '("z" . meow-pop-selection))

(custom-set-variables
 '(meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
 '(meow-select-on-change nil)
 '(meow-use-clipboard t)
 '(meow-keypad-message nil)
 '(meow-cursor-type-motion '(hbar . 2))
 '(meow-replace-state-name-list
   '((normal . "[N]")
     (motion . "[M]")
     (keypad . "[K]")
     (insert . "[I]")
     (beacon . "[B]"))))

(meow-global-mode 1)

(unless (alist-get ?f meow-char-thing-table)
  (meow-tree-sitter-register-defaults))

(add-to-list 'meow-char-thing-table '(?x . list))

(meow-thing-register
 'list
 #'puni-bounds-of-list-around-point
 #'puni-bounds-of-sexp-around-point)

;;; meow-conf.el ends here
