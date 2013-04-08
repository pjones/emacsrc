;;; haskell-mode-conf.el -- Settings for Haskell mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'cl)
  (require 'haskell-indentation)
  (require 'haskell-mode))

(defun pjones:haskell-find-cabal-file ()
  "Return the directory containing a *.cabal file or the current
directory."
  (let ((dir default-directory))
    (while (and (not (string= dir "/"))
                (not (file-expand-wildcards (concat dir "*.cabal"))))
      (setq dir (file-name-directory (substring dir 0 -1))))
    (if (string= dir "/") default-directory dir)))

(defun pjones:haskell-sort-imports ()
  "If point is in a block of import statements then sort them.
Otherwise go totally crazy."
  (interactive)
  (let ((b (save-excursion
             (move-beginning-of-line nil)
             (while (looking-at "^import") (forward-line -1))
             (forward-line 1)
             (point)))
        (e (save-excursion
             (move-beginning-of-line nil)
             (while (looking-at "^import") (forward-line 1))
             (forward-line -1)
             (point))))
    (sort-regexp-fields
     nil "^import \\(qualified \\)?\\(.+\\)$" "\\2" b e)))

(defun pjones:haskell-new-import (&optional qualified)
  "Add a new import statement up along with the other import
statements.  If no other imports exist add it after the first
module line, failing that add it at the top of the buffer.

With a prefix argument make the import qualified."
  (interactive "P")
  (let* ((prompt  (if qualified "import qualified: " "import: "))
         (default (if qualified "Data.Text as T" "Data.Text (Text)"))
         (module (read-string prompt nil nil default)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^module")
      (re-search-forward "^import")
      (move-end-of-line 1)
      (newline)
      (insert (concat "import " (if qualified "qualified ")))
      (insert module)
      (pjones:haskell-sort-imports))))

(defun pjones:haskell-lint-all ()
  "Run hlint from a directory containing a .cabal file."
  (interactive)
  (let* ((dir (pjones:haskell-find-cabal-file))
         (files (cl-remove-if
                 (lambda (f)
                   (or (string= "."         (file-name-base f))
                       (string= ".."        (file-name-base f))
                       (string= "cabal-dev" (file-name-base f))
                       (and (not (file-directory-p f))
                            (not (string= "hs" (file-name-extension f))))))
                 (directory-files dir t))))
    (haskell-check (concat "hlint " (mapconcat 'identity files " ")))))

(defun pjones:haskell-mode-hook ()
  (pjones:prog-mode-hook)
  (local-set-key (kbd "RET")     'newline-and-indent)
  (local-set-key (kbd "C-c C-a") 'pjones:haskell-new-import)
  (local-set-key (kbd "C-c C-s") 'pjones:haskell-sort-imports)
  (local-set-key (kbd "C-c C-v") 'pjones:haskell-lint-all)
  (make-local-variable 'tab-always-indent)
  (setq tab-always-indent t
        haskell-indentation-layout-offset 2
        haskell-indentation-starter-offset 2
        haskell-indentation-left-offset 2
        haskell-indentation-ifte-offset 2
        haskell-indentation-where-pre-offset 2
        haskell-indentation-where-post-offset 2)
  (turn-on-haskell-indentation))

(add-hook 'haskell-mode-hook 'pjones:haskell-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
