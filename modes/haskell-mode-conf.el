;;; haskell-mode-conf.el -- Settings for Haskell mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'cl)
  (require 'haskell-indentation)
  (require 'haskell-mode))

;;; TO-DO List:
;;
;; * Add support for camelcase to keys like C-w
;; * Fix the auto-indenting in comments!!!
;; * Figure out a better way to do completions.
;; * Need a better key for completions.
;; * Remove whitespace at end of line while typing?


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
             (move-end-of-line nil)
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

(defun pjones:haskell-smart-newline ()
  "Insert a new line below point that looks like the current
line.  Examples:

  * List item markup in comments
  * Applicative <$> and <*> lines
  * Lines that should begin with a pipe
  * Lines that should begin with a comma (export list, list literals)
  * Duplicate what's on the line up to a =
  * Another import statement
  * Everything on the current line up to point"
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (cond ((looking-at "\\(--\\s-+\\*\\)")
           (end-of-line)
           (newline)
           (insert (concat (match-string-no-properties 1) " ")))
          ((looking-at "\\(.*<\\$>\\|\\s-+<\\*>\\)")
           (end-of-line)
           (newline)
           (insert (make-string (- (length (match-string-no-properties 1)) 3) ? ))
           (insert "<*> "))
          ((looking-at "\\(data.*=\\|\\s-+|\\)")
           (end-of-line)
           (newline)
           (insert (make-string (- (length (match-string-no-properties 1)) 1) ? ))
           (insert "| "))
          ((looking-at "\\(\\s-+[,(]\\|.*[{\\[]\\)")
           (end-of-line)
           (newline)
           (insert (make-string (- (length (match-string-no-properties 1)) 1) ? ))
           (insert ", "))
          ((looking-at "\\(.*=\\)")
           (end-of-line)
           (newline)
           (insert (concat (match-string-no-properties 1) " ")))
          ((looking-at "import\\s-")
           (end-of-line)
           (newline)
           (insert "import "))
          (t
           (let ((text (buffer-substring (point) pt)))
             (end-of-line)
             (newline)
             (insert text))))))

(defun pjones:haskell-mode-hook ()
  "Hook run on new Haskell buffers."
  (pjones:prog-mode-hook)
  (turn-on-haskell-indentation)

  ;; Undo some stupid haskell-mode bindings.
  (let ((map haskell-indentation-mode-map))
    (define-key map (kbd "RET")   'newline-and-indent)
    (define-key map [?\r]         'newline-and-indent)
    (define-key map [backspace]   'backward-delete-char-untabify))

  ;; And add some of my own
  (local-set-key (kbd "C-c C-a") 'pjones:haskell-new-import)
  (local-set-key (kbd "C-c C-s") 'pjones:haskell-sort-imports)
  (local-set-key (kbd "C-c C-v") 'pjones:haskell-lint-all)
  (local-set-key (kbd "M-RET")   'pjones:haskell-smart-newline)

  (make-local-variable 'tab-always-indent)
  (setq tab-always-indent t
        haskell-indentation-layout-offset 2
        haskell-indentation-starter-offset 2
        haskell-indentation-left-offset 2
        haskell-indentation-ifte-offset 2
        haskell-indentation-where-pre-offset 2
        haskell-indentation-where-post-offset 2))

(add-hook 'haskell-mode-hook 'pjones:haskell-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
