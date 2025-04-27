;;; code.el -- Settings and functions for programming modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(declare-function dumb-jump-xref-activate "dumb-jump")
(declare-function flycheck-mode "flycheck")
(declare-function indium-connect-to-chrome "indium")
(declare-function indium-run-node "indium")
(declare-function puni-mode "puni")
(declare-function s-trim "s")
(declare-function yas-minor-mode "yasnippet")

(defun pjones:comment-bar ()
  "Create a comment bar based on the current mode."
  (interactive)
  (require 's)
  (let* ((cs (s-trim comment-start))
         (col (current-column))
         (info (cond
                ((string= cs "--") '(?- "-"  ""))
                ((string= cs "//") '(?* "/*" "*/"))
                ((string= cs "/*") '(?* "/*" "*/"))
                ((string= cs "#")  '(?# "#"  "#"))
                (t (list ?# comment-start
                         (if (> (length comment-end) 0)
                             comment-end
                           "")))))
         (leading (buffer-substring
                   (save-excursion
                     (beginning-of-line)
                     (point))
                   (point)))
         (go (lambda ()
               (let ((char  (nth 0 info))
                     (start (nth 1 info))
                     (end   (nth 2 info)))
                 (insert start)
                 (insert-char char (- 80 (length start) (length end) col))
                 (insert end)))))
    (if (string-match-p "^\\s-*$" leading)
        (progn
          (funcall go)
          (newline)
          (insert leading))
      (save-excursion
        (back-to-indentation)
        (setq col (current-column))
        (beginning-of-line)
        (open-line 1)
        (insert-char ?  col)
        (funcall go)))))

(defun pjones:prog-mode-hook ()
  "Settings and bindings for programming modes."
  (keymap-local-set "C-<tab>" #'pjones:comment-bar)

  (setq-local
   comment-auto-fill-only-comments t)   ; Don't auto fill code.

  (auto-fill-mode)
  (display-line-numbers-mode)
  (flycheck-mode)
  (puni-mode)
  (save-place-mode)
  (yas-minor-mode)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 50 t)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p 0 t))

;; Hook In:
(add-hook 'prog-mode-hook #'pjones:prog-mode-hook)

(defun pjones:indium-start-chrome ()
  "Start an Indium session for Chrome."
  (interactive)
  (pjones:indium-start "chrome"))

(defun pjones:indium-start-node ()
  "Start an Indium session for Node.js."
  (interactive)
  (pjones:indium-start "node"))

(defun pjones:indium-start (type)
  "Start an Indium process for TYPE."
  (require 'indium)
  (cond
   ((get-buffer "*JS REPL*")
    (switch-to-buffer "*JS REPL*"))
   ((string= type "chrome")
    (indium-connect-to-chrome))
   ((string= type "node")
    (indium-run-node "node"))))

;;; code.el ends here
