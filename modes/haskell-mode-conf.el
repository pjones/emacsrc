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
  "DEL i" #'haskell-navigate-imports
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

(defun pjones:hasky-extensions ()
  "Wrapper around `hasky-extensions'.
A version of `hasky-extensions' that doesn't use avy."
  (interactive)
  (let* ((exts hasky-extensions)
         (active (hasky-extensions-list))
         (name (ivy-completing-read "Extension: " exts nil t)))
    (if (member name active) (hasky-extensions-remove name)
      (hasky-extensions-add name))))

(defun pjones:hoogle ()
  "Run hoogle and display results in ivy."
  (interactive)
  (let ((query (read-string "Query: " (thing-at-point 'symbol t))) result)
    (with-temp-buffer
      (call-process "hoogle" nil t nil "search" "--link" query)
      (goto-char (point-min))
      (while (search-forward-regexp "^\\(.*\\) -- \\(.*\\)$" nil t)
        (setq result (cons `(,(match-string 1) . ,(match-string 2)) result))))
    (let* ((choice (ivy-completing-read "Matches: " result nil t))
           (url (and choice (cdr (assoc choice result)))))
      (when url
        (w3m-goto-url (if (string-match-p "/$" url)
                          (concat url "index.html")
                        url))))))

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

  ;; Pretty symbols:
  ;; https://gist.github.com/m-renaud/2c085d453b1263f1a6ed52d0c90688de
  (setq prettify-symbols-alist
    '(("forall" . ?∀)
      (">>=" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
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
          company-keywords company-dabbrev))))

(defun pjones:dante-mode-hook ()
  "Peter's hook for Dante."
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(add-hook 'haskell-mode-hook       #'pjones:haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook #'highlight-indent-guides-mode)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)
(add-hook 'dante-mode-hook         #'pjones:dante-mode-hook)

;;; haskell-mode-conf.el ends here
