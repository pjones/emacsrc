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

(defun pjones:meow-insert-line ()
  "Move to beginning of line and enter insert mode."
  (interactive)
  (when (use-region-p)
    (meow--direction-backward)
    (meow--cancel-selection))
  (back-to-indentation)
  (meow--switch-state 'insert))

(defun pjones:meow-append-line ()
  "Move to end of line and enter insert mode."
  (interactive)
  (when (use-region-p)
    (meow--direction-forward)
    (meow--cancel-selection))
  (end-of-line)
  (meow--switch-state 'insert))

(defun pjones:meow-insert-select ()
  "Enter insert mode and select symbol at point."
  (interactive)
  (meow-insert-exit)
  (call-interactively #'meow-mark-symbol))

(defun pjones:meow-change-to-line-end ()
  "Kill line then enter insert mode."
  (interactive)
  (kill-line)
  (meow--switch-state 'insert))

(defun pjones:isearch-forward-thing-at-point ()
  "Use function `isearch-forward-thing-at-point' with `repeat-mode'."
  (interactive)
  (isearch-forward-thing-at-point)
  (setq this-command 'isearch-repeat-forward
        last-command-event ?\C-s))

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
(dolist (entry '((mu4e-main-mode . motion)
                 (mu4e-view-mode . motion)
                 (vterm-mode     . insert)))
  (add-to-list 'meow-mode-state-list entry))

;; Like the existing key maps used as the parent, except some bindings
;; have been moved around so they work with meow's keypad mode without
;; needing to use the space key all the time.
(defvar-keymap pjones:ctl-x-map
  :doc "C-x map with some things moved around."
  :parent ctl-x-map
  "C-b" (keymap-lookup ctl-x-map "b")
  "b" (keymap-lookup ctl-x-map "C-b"))

(defvar-keymap pjones:project-map
  :doc "Project map with easier keypad bindings."
  :parent (keymap-lookup ctl-x-map "p")
  "C-b" #'project-switch-to-buffer
  "b"   #'project-list-buffers)

;; Give meow a private version of `mode-specific-map' so that it
;; doesn't alter the existing map.
(defvar-keymap pjones:mode-specific-map
  :doc "Private C-c map for meow."
  :parent mode-specific-map)

(setopt meow-keymap-alist
        (append `((leader . ,pjones:mode-specific-map))
                (assq-delete-all 'leader meow-keymap-alist)))

(meow-motion-define-key
 '("<escape>" . ignore))

(meow-leader-define-key
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
 '("8" . meow-digit-argument)
 '("r" . "C-x r")
 '("w" . "C-x w")
 '("z" . "C-z")
 (cons "p" pjones:project-map)
 (cons "x" pjones:ctl-x-map))

(meow-normal-define-key
 '("-" . negative-argument)
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
 '("a" . meow-append)
 '("A" . pjones:meow-append-line)
 '("B" . meow-back-symbol)
 '("b" . meow-back-word)
 '("C" . pjones:meow-change-to-line-end)
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
 '("I" . pjones:meow-insert-line)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . pjones:isearch-forward-thing-at-point)
 '("o" . meow-open-above)
 '("O" . meow-open-below)
 '("q" . pjones:meow-quit)
 '("r a" . meow-beginning-of-thing)
 '("r e" . meow-end-of-thing)
 '("r i" . meow-inner-of-thing)
 '("r o" . meow-bounds-of-thing)
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
 '(meow-keypad-leader-dispatch nil)
 '(meow-select-on-change nil)
 '(meow-use-clipboard t)
 '(meow-keypad-message nil)
 '(meow-cursor-type-motion '(hbar . 2))
 '(meow-expand-hint-remove-delay 0)
 '(meow-keypad-start-keys '((?c . ?c) (?h . ?h)))
 '(meow-replace-state-name-list
   '((normal . "[N]")
     (motion . "[M]")
     (keypad . "[K]")
     (insert . "[I]")
     (beacon . "[B]"))))

(keymap-global-set "C-c n" meow-normal-state-keymap)

(meow-global-mode 1)

(unless (alist-get ?f meow-char-thing-table)
  (meow-tree-sitter-register-defaults))

(add-to-list 'meow-char-thing-table '(?x . list))

(meow-thing-register
 'list
 #'puni-bounds-of-list-around-point
 #'puni-bounds-of-sexp-around-point)

;;; meow-conf.el ends here
