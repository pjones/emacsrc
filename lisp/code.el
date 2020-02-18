;;; code.el -- Settings and functions for programming modes
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'saveplace)
  (require 'company))

;;; Dependencies:
(require 'projectile)

;; Create some faces
(defface pjones:fixme-face
  '((t (:inherit 'font-lock-warning-face)))
  "Face to style FIXME and TODO with."
  :group 'faces)

(defun pjones:compilation-buffer-name-function (mode-name)
  "Per-project compilation buffers for MODE-NAME."
  (concat "*" (downcase mode-name)
          (if (projectile-project-p)
              (concat ":" (projectile-project-name))
            "")
          "*"))

(defun pjones:projectile-compile-project (ask)
  "Compile a project forcing a unique compilation buffer.

If ASK is non-nil, prompt for a compile command even if it has
already been cached."
  (interactive "P")
  (let* ((compilation-buffer-name-function 'pjones:compilation-buffer-name-function)
         (default-directory (projectile-compilation-dir))
         (default-command (projectile-compilation-command default-directory))
         (compilation-read-command nil)
         (compile-command (if (or ask (null default-command))
                              (compilation-read-command default-command)
                            default-command)))
    (save-excursion
      (puthash default-directory compile-command projectile-compilation-cmd-map)
      (compile compile-command))))

(defun pjones:comment-bar ()
  "Create a comment bar based on the current mode."
  (interactive)
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

(defun pjones:add-fixme-lock ()
  "Add todo markers as keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\|NOTE:\\)"
                                 1 'pjones:fixme-face t))))
(defun pjones:prog-mode-hook ()
  "Settings and bindings for programming modes."
  ;; Completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-dabbrev-code
                                   company-etags
                                   company-keywords))

  (setq comment-empty-lines t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (local-set-key (kbd "C-<tab>") 'pjones:comment-bar)
  (show-paren-mode)
  (whitespace-mode)
  (auto-fill-mode)
  (flyspell-prog-mode)
  (save-place-mode)
  (display-line-numbers-mode)
  (pjones:add-fixme-lock)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(defun pjones:add-programming-hook (mode-hook)
  "Add the programming hook to the given MODE-HOOK."
  (add-hook mode-hook 'pjones:prog-mode-hook))

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
