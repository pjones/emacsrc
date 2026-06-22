;;; code.el -- Settings and functions for programming modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 's)

(declare-function dumb-jump-xref-activate "dumb-jump")
(declare-function indium-connect-to-chrome "indium")
(declare-function indium-run-node "indium")
(declare-function outline-indent-minor-mode "outline-indent")
(declare-function pjones:delete-whitespace-mode "./whitespace.el")
(declare-function puni-mode "puni")
(declare-function s-trim "s")
(declare-function yas-minor-mode "yasnippet")

(defun pjones:comment-bar-str ()
  "Return a comment bar string."
  (let* ((cs (s-trim comment-start))
         (col (current-column))
         (info (cond
                ((string= cs "--") '("-" "-"  ""))
                ((string= cs "//") '("*" "/*" "*/"))
                ((string= cs "/*") '("*" "/*" "*/"))
                ((string= cs "#")  '("#" "#"  "#"))
                (t (list "#" comment-start
                         (if (> (length comment-end) 0)
                             comment-end
                           "")))))
         (spacer (nth 0 info))
         (start (nth 1 info))
         (end   (nth 2 info)))
    (concat start
            (s-repeat (- 80 (length start) (length end) col) spacer)
            end)))

(defun pjones:comment-bar ()
  "Create a comment bar based on the current mode."
  (interactive)
  (let* ((col (current-column))
         (leading (buffer-substring
                   (save-excursion
                     (beginning-of-line)
                     (point))
                   (point))))
    (if (string-match-p "^\\s-*$" leading)
        (progn
          (insert (pjones:comment-bar-str))
          (newline)
          (insert leading))
      (save-excursion
        (back-to-indentation)
        (setq col (current-column))
        (beginning-of-line)
        (open-line 1)
        (insert-char ?  col)
        (insert (pjones:comment-bar-str))))))

(defun pjones:prog-mode-hook ()
  "Settings and bindings for programming modes."
  (keymap-local-set "C-<tab>" #'pjones:comment-bar)

  (setq-local
   comment-auto-fill-only-comments t)   ; Don't auto fill code.

  (auto-fill-mode)
  (flymake-mode)
  (outline-indent-minor-mode)
  (pjones:delete-whitespace-mode)
  (puni-mode)
  (save-place-mode)
  (yas-minor-mode)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 50 t)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p 0 t)

  ;; Some modes (csharp-mode) put `t' in the backend list!
  (setq xref-backend-functions
        (-filter
         (lambda (elm) (not (equal t elm)))
         xref-backend-functions)))

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

;; Add rules for finding sibling files:
(rx-let ((file (+ (not ?/)))
         (c-ext (seq ".c" (? "pp") eos))
         (h-ext (seq ".h" eos))
         (file-sans-ext (ext) (seq (group file) ext))
         (path-sans-ext (path ext) (seq (group "/" (+? anychar) "/") path (group (+? anychar) "/" file) ext)))
  (let ((rules
         `(;; Find header file in the same directory:
           (,(rx (file-sans-ext c-ext)) "\\1.h")

           ;; Find source file in the same directory:
           (,(rx (file-sans-ext h-ext)) "\\1.cpp")

           ;; Find source file outside of the include directory.
           (,(rx (file-sans-ext h-ext)) "../../src/\\1.cpp")

           ;; OpenMS source file to header:
           (,(rx (path-sans-ext "src/openms/source/" c-ext))
            "\\1src/openms/include/OpenMS/\\2.h")

           ;; OpenMS header file to source:
           (,(rx (path-sans-ext "src/openms/include/OpenMS/" h-ext))
            "\\1src/openms/source/\\2.cpp")

           ;; End of rules.
           )))
    (dolist (rule rules)
      (add-to-list 'find-sibling-rules rule))))

;;; code.el ends here
