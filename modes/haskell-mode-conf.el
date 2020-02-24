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

(require 'dante)
(require 'direnv)
(require 'evil-leader)
(require 'flycheck)
(require 'haskell-mode)
(require 'hasky-extensions)
(require 'highlight-indent-guides)

(declare-function pjones:prog-mode-hook "../lisp/code.el")

;; Settings for haskell-mode and friends:
(custom-set-variables
  '(haskell-stylish-on-save nil)
  '(haskell-tags-on-save t)
  '(haskell-completing-read-function 'ivy-completing-read)
  '(dante-repl-command-line nil))

;; A few extra key bindings:
(evil-leader/set-key-for-mode 'haskell-mode
  "DEL e" #'haskell-cabal-visit-file
  "DEL h" #'pjones:hoogle
  "DEL I" #'pjones:haskell-add-import
  "DEL i" #'pjones:haskell-add-import-from-hoogle
  "DEL j" #'haskell-navigate-imports
  "DEL q" #'pjones:haskell-toggle-qualified
  "DEL s" #'pjones:haskell-sort-imports
  "DEL t" #'dante-type-at
  "DEL x" #'pjones:hasky-extensions
  "DEL y" #'dante-info)

(evil-leader/set-key-for-mode 'haskell-cabal-mode
  "DEL s" #'haskell-cabal-subsection-arrange-lines)

(evil-define-key 'normal haskell-cabal-mode-map "gj" #'haskell-cabal-next-section)
(evil-define-key 'normal haskell-cabal-mode-map "gk" #'haskell-cabal-previous-section)

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
  (save-excursion
    (goto-char (point-max))
    (haskell-navigate-imports)
    (insert (pjones:haskell-read-import initial))
    (insert "\n")
    (haskell-sort-imports)))

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

(defun pjones:haskell-mode-hook ()
  "Hook run on new Haskell buffers."
  ;; Update environment variables (i.e. PATH) first!
  (direnv-update-environment)

  ;; Boot `haskell-mode':
  (haskell-indentation-mode)
  (dante-mode)

  ;; Load helper packages:
  (pjones:prog-mode-hook)
  (flycheck-mode)
  (subword-mode)
  (abbrev-mode)
  (highlight-indent-guides-mode)

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
                     (Bc . Bl) ?> (Br . Br) ?>))))

  ;; Configure completion specific to this mode.  I *think* that Dante
  ;; inserts itself in this list in a way that prevents merging with
  ;; other backends and thus breaks completion.  So, I overwrite what
  ;; Dante does to fix things.
  (set (make-local-variable 'company-backends)
       '((dante-company company-capf company-files
          company-dabbrev-code company-gtags company-etags
          company-keywords company-dabbrev)))

  ;; Get tags working correctly:
  (xref-etags-mode)
  (setq xref-backend-functions '(etags--xref-backend dante--xref-backend)))

(defun pjones:dante-mode-hook ()
  "Peter's hook for Dante."
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(add-hook 'haskell-mode-hook       #'pjones:haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook #'highlight-indent-guides-mode)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)
(add-hook 'dante-mode-hook         #'pjones:dante-mode-hook)

;;; haskell-mode-conf.el ends here
