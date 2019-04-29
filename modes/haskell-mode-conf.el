;;; haskell-mode-conf.el -- Settings for Haskell mode.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'cl))

(require 'haskell-mode)
(require 'company-ghc)
(require 'dante)
(require 'flycheck)
(require 'ghc)
(require 'haskell)

(declare-function pjones:prog-mode-hook "../lisp/code.el")
(declare-function pjones:define-keys-from-hydra "../lisp/functions.el")

;; Settings for haskell-mode and friends:
(custom-set-variables
  '(haskell-stylish-on-save nil)
  '(haskell-tags-on-save nil)
  '(haskell-completing-read-function 'ido-completing-read)
  '(shm-auto-insert-skeletons nil)
  '(dante-repl-command-line '("nix-hs" "repl")))

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
  (save-mark-and-excursion
    (goto-char (point-min))
    (let ((start (search-forward-regexp "module "))
          (end (search-forward-regexp "[[:space:]]")))
      (buffer-substring start (- end 2)))))

(defun pjones:haskell-module-name-to-kill-ring ()
  "Save the module name of the current buffer to the kill ring."
  (interactive)
  (kill-new (pjones:haskell-module-name)))

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


(defun pjones-haskell-process-wrapper-function (argv)
  "Run Haskell tools through nix-shell by modifying ARGV.
See `haskell-process-wrapper-function' for details."
  (append (list "nix-hs")
          (list (mapconcat 'identity argv " "))))

(defun pjones:haskell-mode-hook ()
  "Hook run on new Haskell buffers."
  (make-local-variable 'tab-always-indent)

  ;; These need to be set before calling `pjones:prog-mode-hook'.
  (setq tab-always-indent t
        beginning-of-defun-function 'pjones:haskell-beginning-of-defun
        end-of-defun-function 'pjones:haskell-end-of-defun)

  ;; Indentation.
  (haskell-indentation-mode -1)
  (structured-haskell-mode)

  (pjones:prog-mode-hook)
  (subword-mode)
  (abbrev-mode)

  ;; Linting and checking:
  (dante-mode)
  (flycheck-mode)
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))

  ;; Configure completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-ghc company-dabbrev company-abbrev))

  ;; A few extra key bindings:
  (let ((map haskell-mode-map))
    (define-key map (kbd "C-c C-e") #'pjones:haskell-edit-cabal-file)
    (define-key map (kbd "C-c C-s") #'pjones:haskell-sort-imports)))

(defun pjones:dante-mode-hook ()
  "Peter's hook for Dante."
  (let ((map dante-mode-map))
    (define-key map (kbd "C-c ,") nil)))

(defun pjones:structured-haskell-mode-hook ()
  "Peter's hook for `structured-haskell-mode'."
  (let ((map shm-map))
    (define-key map (kbd "(") nil)
    (define-key map (kbd ")") nil)
    (define-key map (kbd "[") nil)
    (define-key map (kbd "]") nil)
    (define-key map (kbd "{") nil)
    (define-key map (kbd "}") nil)
    (define-key map (kbd "C-c C-e") nil)))

(add-hook 'haskell-mode-hook #'pjones:haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)
(add-hook 'dante-mode-hook #'pjones:dante-mode-hook)
(add-hook 'structured-haskell-mode-hook #'pjones:structured-haskell-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
