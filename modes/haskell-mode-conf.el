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
(require 'haskell)
(require 'haskell-interactive-mode)
(require 'haskell-mode)
(require 'haskell-process)

(declare-function pjones:prog-mode-hook "../lisp/code.el")

;; Settings for haskell-mode and friends:
(custom-set-variables
  '(haskell-stylish-on-save nil)
  '(haskell-tags-on-save t)
  '(haskell-completing-read-function 'ivy-completing-read)
  '(haskell-process-type 'cabal-new-repl)
  '(haskell-process-suggest-hoogle-imports t)
  '(dante-repl-command-line '("cabal" "new-repl")))

;; A few extra key bindings:
(evil-leader/set-key-for-mode 'haskell-mode
  "SPC e" #'haskell-cabal-visit-file
  "SPC i" #'haskell-navigate-imports
  "SPC s" #'haskell-sort-imports
  "SPC t" #'dante-info)

;; This overwrite fixes a bug where imports are not sorted because I
;; put a comment line above them.
(defun haskell-sort-imports-goto-group-start ()
  "Overwrite the version from haskell-sort-imports.el."
  (while (looking-at "^import") (forward-line -1))
  (forward-line 1))

(defun pjones:haskell-mode-hook ()
  "Hook run on new Haskell buffers."
  ;; Update environment variables (i.e. PATH) first!
  (direnv-update-environment)

  ;; Boot `haskell-mode':
  (haskell-indentation-mode)
  (interactive-haskell-mode)
  (haskell-process-load-file)

  ;; Configure completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-capf company-dabbrev company-abbrev))

  ;; Load helper packages:
  (pjones:prog-mode-hook)
  (flycheck-mode)
  (dante-mode)
  (subword-mode)
  (abbrev-mode))

(defun pjones:dante-mode-hook ()
  "Peter's hook for Dante."
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(add-hook 'haskell-mode-hook       #'pjones:haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)
(add-hook 'dante-mode-hook         #'pjones:dante-mode-hook)

;;; haskell-mode-conf.el ends here
