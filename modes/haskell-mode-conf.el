;;; haskell-mode-conf.el -- Settings for Haskell mode.
;;
;;; Commentary:
;;
;; My goal with this configuration is to reduce the write->compile
;; cycle with Haskell code, without requiring a ton of dependencies or
;; complicated set up.  I'm also not interested in fancy completion or
;; refactoring tools.
;;
;;; Code:
(eval-when-compile
  (require 'cl))

(require 'align)
(require 'direnv)
(require 'eglot)
(require 'evil-leader)
(require 'haskell-mode)
(require 'hasky-extensions)
(require 'highlight-indent-guides)

(declare-function pjones:prog-mode-hook "../lisp/code.el")

;; Settings for haskell-mode and friends:
(custom-set-variables
  '(haskell-stylish-on-save t)
  '(haskell-mode-stylish-haskell-path "ormolu")
  '(haskell-mode-stylish-haskell-args nil)
  '(haskell-tags-on-save t)
  '(haskell-completing-read-function 'ivy-completing-read))

;; A few extra key bindings:
(evil-leader/set-key-for-mode 'haskell-mode
  "DEL e"   #'haskell-cabal-visit-file
  "DEL h"   #'pjones:hoogle
  "DEL i i" #'pjones:haskell-add-import
  "DEL i h" #'pjones:haskell-add-import-from-hoogle
  "DEL i p" #'pjones:haskell-import-project-file
  "DEL j e" #'pjones:haskell-navigate-exports
  "DEL j i" #'haskell-navigate-imports
  "DEL j b" #'pjones:haskell-navigate-imports-return
  "DEL q"   #'pjones:haskell-toggle-qualified
  "DEL s"   #'pjones:haskell-sort-imports
  "DEL t"   #'eglot-help-at-point
  "DEL x"   #'pjones:hasky-extensions
  "DEL y m" #'pjones:haskell-kill-module-name)

(evil-leader/set-key-for-mode 'haskell-cabal-mode
  "DEL s" #'haskell-cabal-subsection-arrange-lines)

(evil-define-key 'normal haskell-cabal-mode-map "gj" #'haskell-cabal-next-section)
(evil-define-key 'normal haskell-cabal-mode-map "gk" #'haskell-cabal-previous-section)

(add-to-list 'align-rules-list
  '(haskell-types
    (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
    (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
  '(haskell-assignment
    (regexp . "\\(\\s-+\\)=\\s-+")
    (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
  '(haskell-arrows
    (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
    (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
  '(haskell-left-arrows
    (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
    (modes quote (haskell-mode literate-haskell-mode))))

;; This overwrite fixes a bug where imports are not sorted because I
;; put a comment line above them.
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

;; Redefine the existing function:
(defalias 'haskell-sort-imports #'pjones:haskell-sort-imports)

(defun pjones:hasky-extensions ()
  "Wrapper around `hasky-extensions'.
A version of `hasky-extensions' that doesn't use avy."
  (interactive)
  (let* ((exts hasky-extensions)
         (active (hasky-extensions-list))
         (name (ivy-completing-read "Extension: " exts nil t)))
    (if (member name active) (hasky-extensions-remove name)
      (hasky-extensions-add name))))

(defun pjones:haskell-generate-module-name (&optional file)
  "Attempt to turn FILE into a module name."
  (let* ((file-name (or file buffer-file-name))
         (cabal-dir (file-name-directory (haskell-cabal-find-file)))
         (name (s-chop-suffix ".hs" (s-chop-prefix cabal-dir file-name))))
    (s-replace-all '(("/" . "."))
      (s-chop-prefixes (list "src/" "test/" "app/") name))))

(defun pjones:haskell-search-hoogle (prompt)
  "Use PROMPT to prompt the user and search hoogle.
The match chosen by the user will be returned."
  (let ((query (read-string prompt (thing-at-point 'symbol t))) result)
    (with-temp-buffer
      (call-process "hoogle" nil t nil "search" "--link" "--count=30" query)
      (goto-char (point-min))
      (while (search-forward-regexp "^\\(.*\\) -- \\(.*\\)$" nil t)
        (setq result (cons `(,(match-string 1) . ,(match-string 2)) result))))
    (cons (assoc (ivy-completing-read "Matches: " result nil t) result)
          (list query))))

(defun pjones:hoogle ()
  "Run hoogle and display results in ivy."
  (interactive)
  (let* ((choice (pjones:haskell-search-hoogle "Query: "))
         (url (and choice (cdar choice))))
    (when url
      (eww-browse-url
       (if (string-match-p "/$" url) (concat url "index.html") url) t)
      (pjones:eww-rename-buffer))))

(defun pjones:haskell-toggle-qualified nil
  "Toggle the 'qualified' modifier on the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((bol (point))
          (eol (save-excursion
                 (end-of-line)
                 (point))))
      (cond
       ((search-forward-regexp
         (rx ;; A qualified import with optional "as".
          (and (group "import" (1+ blank))
               (and "qualified" (1+ blank))
               (group (1+ (in alnum ?.)))
               (? (1+ blank) "as" (1+ blank) (1+ (in alnum ?.)))))
         eol t)
        (replace-match "\\1\\2" t))
       ((search-forward-regexp
         (rx ;; An unqualified import.
          (and (group "import" (1+ blank))   ;; Group 1
               (group (+ (in alnum ?.)) ?.) ;; Group 2
               (group (+ (in alnum)))       ;; Group 3
               (group (* blank))))          ;; Group 4
         eol t)
        (replace-match "\\1qualified \\2\\3 as \\3\\4" t))))))

(defun pjones:haskell-read-import (&optional module)
  "Read a Haskell import line from the minibuffer.
If non-nil, use MODULE as the initial module name."
  (let ((initial-input (concat "import " module (if module "")))
        (map (make-sparse-keymap "pjones-import")))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-q") #'pjones:haskell-toggle-qualified)
    (read-from-minibuffer "Add Import: " initial-input map)))

(defun pjones:haskell-add-import (&optional initial)
  "Prompt for an import and add it to the imports section.
When prompting, use INITIAL as the initial module name."
  (interactive)
    (goto-char (point-max))
    (haskell-navigate-imports)
    (if initial
        (progn
          (insert (concat (pjones:haskell-read-import initial) "\n"))
          (haskell-sort-imports)
          (haskell-navigate-imports-return))
      (evil-insert-state)
      (yas-expand-snippet (yas-lookup-snippet "import"))))

(defun pjones:haskell-add-import-from-hoogle ()
  "Use hoogle to select an import to add."
  (interactive)
  (let* ((choice (pjones:haskell-search-hoogle "Module Query: "))
         (import (and choice (caar choice))))
    (cond
     ((string-match
       (rx "module"
           (+ blank)
           (group (+ (in alnum ?.)))) import)
      (pjones:haskell-add-import (match-string 1 import)))
     ((string-match
       (rx (group (+ (in alnum ?.)))
           (+ blank)
           (group (+ (not blank)))) import)
      (pjones:haskell-add-import
       (concat (match-string 1 import)
               " (" (match-string 2 import) ")"))))))

(defun pjones:haskell-import-project-file ()
  "Prompt for a project file, then add an import for it."
  (interactive)
  (let* ((project (project-current t))
         (dirs (project-roots project))
         (table (project-file-completion-table project dirs))
         (file (projectile-completing-read "Find file" table)))
    (pjones:haskell-add-import (pjones:haskell-generate-module-name file))))

(defun pjones:haskell-navigate-exports ()
  "Jump to the export list."
  (interactive)
  (setq haskell-navigate-imports-start-point (point))
  (goto-char (point-min))
  (search-forward-regexp "^module ")
  (search-forward-regexp ") where")
  (forward-line -1))

(defun pjones:haskell-navigate-imports-return ()
  "Sort imports then jump back to where we came from."
  (interactive)
  (beginning-of-line)
  (when (looking-at-p "^import ")
    (pjones:haskell-sort-imports))
  (haskell-navigate-imports-return))

(defun pjones:haskell-kill-module-name ()
  "Put the current module name in the kill ring."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^module ")
    (kill-ring-save
     (point)
     (progn
       (search-forward-regexp (rx (or blank eol)))
       (point)))))

(defun pjones:haskell-package-version (&optional cabal-file)
  "Return the version of the package defined in CABAL-FILE."
  (let ((cabal-file (or cabal-file (haskell-cabal-find-file))))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents cabal-file)
        (goto-char (point-min))

        (if (search-forward-regexp
             (rx (and line-start (* blank) "version:" (+ blank)
                      (group (1+ (or digit ?.)))))
             nil nil)
            (buffer-substring-no-properties
             (match-beginning 1)
             (match-end 1))
          "0.1.0.0")))))

(defun pjones:haskell-copyright-text ()
  "Extract the default copyright text from the Setup.hs file."
  (let ((cabal-dir (file-name-directory (haskell-cabal-find-file))))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents (concat cabal-dir "Setup.hs"))
        (goto-char (point-min))
        (if (looking-at-p (rx (and line-start (or "{-|" "-- |"))))
            (progn
              (buffer-substring-no-properties
               (point)
               (progn
                 (search-forward-regexp
                  (rx (and line-start (or "import" "module"))))
                 (end-of-line 0)
                 (point))))
          "-- | Module description.")))))

(defun pjones:haskell-unqualified-module (module)
  "Remove qualified prefix from MODULE."
  (if module
      (let* ((parts (s-split (rx ".") module))
             (name (last parts)))
        (if name (car name) module))
    ""))

(defun pjones:haskell-mode-hook ()
  "Hook run on new Haskell buffers."
  ;; Update environment variables (i.e. PATH) first!
  (direnv-update-environment)

  ;; Boot `haskell-mode':
  (haskell-indentation-mode)

  ;; Load helper packages:
  (pjones:prog-mode-hook)
  (subword-mode)
  (abbrev-mode)
  (highlight-indent-guides-mode)
  (flymake-hlint-load)
  (xref-etags-mode)
  (eglot-ensure)

  ;; Evil doc-lookup (on the "K" key):
  (set (make-local-variable 'evil-lookup-func) #'pjones:hoogle)

  ;; Pretty symbols:
  ;; https://gist.github.com/m-renaud/2c085d453b1263f1a6ed52d0c90688de
  (setq prettify-symbols-alist
    '((">>=" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                     (Bl . Bl) ?> (Bc . Bc) ?>
                     (Bc . Bl) ?= (Br . Br) ?=))
      ("=<<" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                     (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?<
                     (Bl . Bl) ?= (Br . Br) ?<))
      ("=>"  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?=
                     (Br . Br) ?>))
      ("->"  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?-
                     (Bc . Bl) ?- (Br . Br) ?>))
      ("<-"  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?< (Bc . Br) ?- (Bc . Bc) ?-
                     (Bc . Bl) ?- (Br . Br) ?-))
      ("-<"  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?- (Bc . Bc) ?- (Br . Br) ?<))
      (">-"  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?> (Bc . Bc) ?- (Br . Br) ?-))
      ("++"  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?+ (Bc . Br) ?+ (Bc . Bc) ?-
                     (Bc . Bl) ?+ (Br . Br) ?+))
      ("=="  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?=
                     (Bc . Bl) ?= (Br . Br) ?=))
      ("/="  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?/
                     (Bc . Bl) ?= (Br . Br) ?=))
      ("<>"  .  (?\s (Br . Bl) ?\s
                     (Bl . Bl) ?< (Bc . Br) ?<
                     (Bc . Bl) ?> (Br . Br) ?>)))))

(add-hook 'haskell-mode-hook       #'pjones:haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook #'highlight-indent-guides-mode)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)

;; Tell eglot how to start ghcide:
(add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp")))

;;; haskell-mode-conf.el ends here
