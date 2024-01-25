;;; code.el -- Settings and functions for programming modes
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'saveplace))

(declare-function compilation-read-command "compile")
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

(defun pjones:comment-line-break-function (&rest args)
  "Work around a bug in Emacs.
Calls `comment-indent-new-line' with ARGS."
  (let ((comment-auto-fill-only-comments nil))
    (apply #'comment-indent-new-line args)
    (let ((max-backtrack (save-excursion (beginning-of-line) (point))))
      (unless (looking-back "\\s-" max-backtrack)
        (insert " ")))))

(defun pjones:prog-mode-hook ()
  "Settings and bindings for programming modes."
  (require 'flycheck)
  (require 'dumb-jump)
  (setq comment-empty-lines t)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-line-break-function #'pjones:comment-line-break-function)
  (local-set-key (kbd "C-<tab>") 'pjones:comment-bar)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (auto-fill-mode)
  (display-line-numbers-mode)
  (flycheck-mode)
  (flyspell-prog-mode)
  (puni-mode)
  (save-place-mode)
  (whitespace-mode)
  (yas-minor-mode)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

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
