;;; haskell-mode-conf.el -- Settings for Haskell mode.
(eval-when-compile
  (load "../lisp/code.el")
  (load "../lisp/functions.el")
  (require 'cl)
  (require 'ghc)
  (require 'projectile)
  (require 'haskell)
  (require 'haskell-compile)
  (require 'haskell-indentation)
  (require 'haskell-mode))

;; Settings for haskell-mode and friends:
(custom-set-variables
  '(haskell-stylish-on-save nil)
  '(haskell-tags-on-save nil)
  '(haskell-completing-read-function 'ido-completing-read)
  '(haskell-indentation-layout-offset 0)
  '(haskell-indentation-starter-offset 2)
  '(haskell-indentation-left-offset 2)
  '(haskell-indentation-ifte-offset 2)
  '(haskell-indentation-where-pre-offset 2)
  '(haskell-indentation-where-post-offset 2)
  '(haskell-compile-cabal-build-command "make")
  '(haskell-process-type 'stack-ghci)
  '(haskell-process-suggest-language-pragmas t)
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-interactive-popup-errors nil)
  '(haskell-interactive-mode-eval-mode 'haskell-mode)
  '(haskell-process-wrapper-function 'identity))

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

(defun pjones:haskell-compile (&optional arg)
  "Compile the current project using
`haskell-compile-cabal-build-command'.  With ARG tack on the word
test to the compile command."
  (interactive "P")
  (let* ((cabal-file (pjones:haskell-find-cabal-file))
         (default-directory (file-name-directory cabal-file))
         (command haskell-compile-cabal-build-command))
    (compile (if arg (concat command " test") command))))

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

;; TODO:
;; * Remove whitespace at end of line while typing?
;;
;; * which-func-mode
;;     (eval-after-load "which-func"
;;       '(add-to-list 'which-func-modes 'haskell-mode))
;;
;;
;; close frames when using q (think *grep* or *compilation*)

(defhydra hydra-haskell (:hint nil)
  "
^Imports^          ^GHCi^              ^Insert/Edit^          ^Run
^^^^^^^^^-------------------------------------------------------------------------
_C-c C-i_: jump    _C-c C-g_: ghci     _C-c C-0_: cost center  _C-c C-c_: compile
_C-c C-l_: return  _C-c C-r_: reload   _C-c C-p_: new module
_C-c C-s_: sort    _C-c C-t_: type     _C-c C-n_: kill module
^ ^                ^ ^                 _C-c C-e_: edit cabal
"
  ("C-c C-0" haskell-mode-toggle-scc-at-point :color blue)
  ("C-c C-c" pjones:haskell-compile :color blue)
  ("C-c C-e" pjones:haskell-edit-cabal-file :color blue)
  ("C-c C-g" haskell-interactive-switch :color blue)
  ("C-c C-i" haskell-navigate-imports)
  ("C-c C-l" haskell-navigate-imports-return :color blue)
  ("C-c C-n" pjones:haskell-module-name-to-kill-ring :color blue)
  ("C-c C-p" pjones:haskell-new-module :color blue)
  ("C-c C-r" haskell-process-reload)
  ("C-c C-s" pjones:haskell-sort-imports)
  ("C-c C-t" haskell-process-do-type :color blue))

(define-skeleton pjones:haskell-insert-pragma
  "Add the pragma comment syntax." nil "{-# " _ " #-}")

(defun pjones:haskell-nixpkgs-wrapper-function (argv)
  "Function that can be used with the
`haskell-process-wrapper-function' variable to force building
with my custom nix-hs-shell script."
  (append (list "nix-hs-shell" "--run")
          (list (mapconcat 'identity argv " "))))

(defun pjones:start-interactive-haskell-mode ()
  "Hack around a bug in ghc-mod."
  (let* ((cabal-file (pjones:haskell-find-cabal-file))
         (default-directory (file-name-directory cabal-file)))
    (call-process "hs-clone-stack-yaml")
    (interactive-haskell-mode)))

(defun pjones:haskell-mode-hook ()
  "Hook run on new Haskell buffers."
  (make-local-variable 'tab-always-indent)

  ;; These need to be set before calling `pjones:prog-mode-hook'.
  (setq tab-always-indent t
        beginning-of-defun-function 'pjones:haskell-beginning-of-defun
        end-of-defun-function 'pjones:haskell-end-of-defun
        projectile-project-compilation-cmd "make"
        projectile-project-test-cmd        "make test"
        projectile-project-run-cmd         "make run"
        ghc-module-command "ghc-mod")

  (haskell-indentation-mode)
  (pjones:start-interactive-haskell-mode)
  (pjones:prog-mode-hook)
  (subword-mode)
  (abbrev-mode)
  (ghc-init)

  ;; Configure completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-ghc company-dabbrev company-abbrev))

  (define-abbrev-table 'haskell-mode-abbrev-table
    '(("_P" "" pjones:haskell-insert-pragma)))
  (setq local-abbrev-table haskell-mode-abbrev-table)

  ;; Undo some stupid haskell-mode bindings.
  (let ((map haskell-indentation-mode-map))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map [?\r]       'newline-and-indent)
    (define-key map [backspace] 'backward-delete-char-untabify))

  ;; And add some of my own.
  (let ((map haskell-mode-map))
    (define-key map (kbd "C-c h") 'hydra-haskell/body)
    (define-key map (kbd "M-RET") 'pjones:haskell-smart-newline)
    (pjones:define-keys-from-hydra map hydra-haskell/heads))

  ;; And yet another mode map!
  (let ((map interactive-haskell-mode-map))
    (pjones:define-keys-from-hydra map hydra-haskell/heads)))

(add-hook 'haskell-mode-hook 'pjones:haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook 'pjones:prog-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
