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
  "DEL i" #'haskell-navigate-imports
  "DEL s" #'haskell-sort-imports
  "DEL t" #'dante-type-at
  "DEL x" #'pjones:hasky-extensions
  "DEL y" #'dante-info)

(evil-leader/set-key-for-mode 'haskell-cabal-mode
  "DEL s" #'haskell-cabal-subsection-arrange-lines)

(evil-define-key 'normal haskell-cabal-mode-map "gj" #'haskell-cabal-next-section)
(evil-define-key 'normal haskell-cabal-mode-map "gk" #'haskell-cabal-previous-section)

;; This overwrite fixes a bug where imports are not sorted because I
;; put a comment line above them.
(defun haskell-sort-imports-goto-group-start ()
  "Overwrite the version from haskell-sort-imports.el."
  (while (looking-at "^import") (forward-line -1))
  (forward-line 1))

(defun pjones:hasky-extensions ()
  "Wrapper around `hasky-extensions'.
A version of `hasky-extensions' that doesn't use avy."
  (interactive)
  (let* ((exts hasky-extensions)
         (active (hasky-extensions-list))
         (name (ivy-completing-read "Extension: " exts nil t)))
    (if (member name active) (hasky-extensions-remove name)
      (hasky-extensions-add name))))

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
  (abbrev-mode))

(defun pjones:dante-mode-hook ()
  "Peter's hook for Dante."
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(add-hook 'haskell-mode-hook       #'pjones:haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)
(add-hook 'dante-mode-hook         #'pjones:dante-mode-hook)

;;; haskell-mode-conf.el ends here
