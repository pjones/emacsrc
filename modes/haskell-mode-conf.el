;;; haskell-mode-conf.el -- Settings for Haskell mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'cl)
  (require 'haskell-indentation)
  (require 'haskell-mode))

;;; TO-DO List:
;;
;; * Figure out a better way to do completions.
;; * Need a better key for completions.
;; * Remove whitespace at end of line while typing?


(defun pjones:haskell-find-cabal-file ()
  "Return the full path to the *.cabal file for the current project."
  (let* ((dir default-directory)
         (default (concat dir "/" (file-name-base dir) ".cabal"))
         (name))
    (while (and (not (string= dir "/"))
                (not (setq name (file-expand-wildcards (concat dir "?*.cabal")))))
      (setq dir (file-name-directory (substring dir 0 -1))))
    (if (string= dir "/") default (car name))))

(defun pjones:haskell-edit-cabal-file ()
  "Open the project's cabal file for editing."
  (interactive)
  (find-file (pjones:haskell-find-cabal-file)))

(defun pjones:haskell-compile ()
  "Compile the current project using `nix-hs-build'."
  (interactive)
  (let* ((cabal-file (pjones:haskell-find-cabal-file))
         (default-directory (file-name-directory cabal-file))
         (cmd "nix-hs-build"))
    (compile cmd)))

(defun pjones:haskell-beginning-of-defun (&optional arg)
  "Move to the beginning of the current function."
  (dotimes (i (or arg 1))
    (beginning-of-line)
    (while (and (not (bobp)) (or (eolp) (looking-at "^\\s-")))
      (forward-line -1))
    (if (save-excursion (forward-line -1) (looking-at "^\\w"))
        (forward-line -1))) t)

(defun pjones:haskell-end-of-defun (&optional arg)
  "Move to the end of the current function."
  (dotimes (i (or arg 1))
    (beginning-of-line)
    (while (and (not (eobp)) (looking-at "^\\w"))
      (forward-line)) ;; Move past the function name.
    (while (and (not (eobp)) (or (eolp) (looking-at "^\\s-")))
      (forward-line))) t)

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
     nil "^import +\\(qualified \\)?\\(.+\\)$" "\\2" b e)))

(defun pjones:haskell-module-name ()
  "Return the module name for the current buffer."
  (let* ((cabal-path (file-name-directory (pjones:haskell-find-cabal-file)))
         (mod-path   (substring (file-name-sans-extension (buffer-file-name))
                                (length cabal-path)))
         (mod-name   (replace-regexp-in-string "/" "." mod-path t t)))
    (if (string= (substring mod-name 0 4) "src.") (substring mod-name 4)
      mod-name)))

(defun pjones:haskell-new-module ()
  "Write out a blank module line."
  (interactive)
  (let ((mod-name (pjones:haskell-module-name)))
    (insert (concat "module " mod-name " () where\n"))))

(defun pjones:haskell-module-name-to-kill-ring ()
  "Save the module name of the current buffer to the kill ring."
  (interactive)
  (kill-new (pjones:haskell-module-name)))

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
                   (or (string-match "^\\." (file-name-base f))
                       (string= "dist"      (file-name-base f))
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

(defun pjones:haskell-auto-fill-function ()
  "Fix the busted haskell-indentation-auto-fill-function."
  (when (> (current-column) fill-column)
    (while (> (current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (comment-indent-new-line)
    (end-of-line)))

(defun pjones:haskell-mode-hook ()
  "Hook run on new Haskell buffers."
  (make-local-variable 'tab-always-indent)
  (make-local-variable 'normal-auto-fill-function)

  ;; These need to be set before calling `pjones:prog-mode-hook'.
  (setq tab-always-indent t
        normal-auto-fill-function 'pjones:haskell-auto-fill-function
        haskell-indentation-layout-offset 0
        haskell-indentation-starter-offset 2
        haskell-indentation-left-offset 2
        haskell-indentation-ifte-offset 2
        haskell-indentation-where-pre-offset 2
        haskell-indentation-where-post-offset 2
        beginning-of-defun-function 'pjones:haskell-beginning-of-defun
        end-of-defun-function 'pjones:haskell-end-of-defun
        projectile-project-compilation-cmd "nix-hs-build")

  (pjones:prog-mode-hook)
  (subword-mode)

  ;; Undo some stupid haskell-mode bindings.
  (let ((map haskell-indentation-mode-map))
    (define-key map (kbd "RET")   'newline-and-indent)
    (define-key map [?\r]         'newline-and-indent)
    (define-key map [backspace]   'backward-delete-char-untabify))

  ;; And add some of my own
  (let ((map haskell-mode-map))
    (define-key map (kbd "C-c C-a") 'pjones:haskell-new-import)
    (define-key map (kbd "C-c C-c") 'pjones:haskell-compile)
    (define-key map (kbd "C-c C-e") 'pjones:haskell-edit-cabal-file)
    (define-key map (kbd "C-c C-s") 'pjones:haskell-sort-imports)
    (define-key map (kbd "C-c C-v") 'pjones:haskell-lint-all)
    (define-key map (kbd "C-c M-m") 'pjones:haskell-new-module)
    (define-key map (kbd "C-c M-w") 'pjones:haskell-module-name-to-kill-ring)
    (define-key map (kbd "M-RET")   'pjones:haskell-smart-newline)))

(add-hook 'haskell-mode-hook 'pjones:haskell-mode-hook)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-cabal-mode-hook 'pjones:prog-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
