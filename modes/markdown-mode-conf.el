;;; markdown-conf.el -- Settings for markdown-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'company)
(require 'hydra)
(require 'markdown-mode)
(require 'org)
(require 'orgalist)
(require 'whitespace)
(require 'darkroom)

(declare-function pjones:open-line-above "../lisp/interactive.el")
(declare-function pjones:add-fixme-lock "../lisp/code.el")

;; Basic settings.
(custom-set-variables
 '(markdown-reference-location 'end)
 '(markdown-command "nix-shell -p pandoc --run 'pandoc -f markdown -t html'"))

(defvar pjones:markdown-attachments-directory
  "attachments"
  "Name of the directory used to hold git-annex files.")

(defvar pjones:markdown-langs
  '("ruby" "javascript" "html" "css" "haskell" "shell" "python")
  "List of pandoc languages I use.")

(define-skeleton pjones:markdown-slide-notes
  "Add a div section for slide notes." nil
  "<div class=\"notes\">"
  ?\n ?\n _
  ?\n ?\n "</div>")

(defun pjones:markdown-collect-token-names (file)
  "Return a list of Edify token names found in FILE."
  (let ((tokens nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (search-forward-regexp "<<: \\(.+\\)" nil t 1)
        (setq tokens (append tokens (list (match-string 1)))))
      tokens)))

(defun pjones:markdown-slide-fenced-code-block (&optional with-insert with-token)
  "Insert a markdown fenced code block.

If WITH-INSERT is non-nil, prompt for a file path to insert.  If
WITH-TOKEN is non-nil, prompt for a token name."
  (interactive "P")
  (let* ((indent (- (point) (save-excursion (forward-line 0) (point))))
         (prefix (make-string indent ? ))
         (lang (ido-completing-read "Lang: " pjones:markdown-langs))
         (file (when with-insert
                 (file-relative-name
                  (read-file-name "Insert File Path: " nil nil t))))
         (tokens (if (and with-insert with-token) (pjones:markdown-collect-token-names file)))
         (token (if (and with-token tokens) (ido-completing-read "Token: " tokens))))
    (insert (concat "~~~ {." lang
                    (if with-insert (concat " insert=\"" file "\""))
                    (if (and with-token token) (concat " token=\"" token "\""))
                    "}\n" (unless with-insert (concat prefix "\n")) prefix "~~~\n"))
    (forward-line -2)
    (end-of-line)
    t))
(put 'pjones:markdown-slide-fenced-code-block 'no-self-insert t)

(defun pjones:markdown-slide-fenced-code-insert nil
  "Wrapper around `pjones:markdown-slide-fenced-code-block'."
  (interactive)
  (pjones:markdown-slide-fenced-code-block t t))
(put 'pjones:markdown-slide-fenced-code-insert 'no-self-insert t)

(defun pjones:markdown-insert-list-item (&optional arg)
  "Insert a new list item.

Unlike the markdown version of this function, leave a blank line
between the old and new list items.

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
increase the indentation by one level."
  (interactive "p")
  (markdown-insert-list-item arg)
  (pjones:open-line-above t))

(defun pjones:markdown-visual-line ()
  "Don't wrap lines.  Needed for most web forms."
  (interactive)
  (auto-fill-mode -1)
  (whitespace-mode -1)
  (visual-line-mode)
  (visual-fill-mode)
  (darkroom-tentative-mode))

(defun pjones:markdown-follow-thing-at-point (arg)
  "Call (and pass ARG) to `markdown-follow-thing-at-point'."
  (interactive "P")
  (cl-letf (((symbol-function 'find-file)
             (lambda (name) (org-open-file name))))
    (markdown-follow-thing-at-point arg)))

(defun pjones:markdown-attach-file (file &optional name)
  "Attach FILE to the current document.

The given FILE will be attached to the current document by adding
it as a git-annex file in the `pjones:markdown-attachments-directory'
directory.  Optionally renaming FILE to NAME."
  (interactive
   (let* ((f (read-file-name "Attach: " nil nil t))
          (n (if current-prefix-arg
                 (read-string "New Name: " (file-name-nondirectory f)))))
     (list f n)))
  (let* ((name (or name (file-name-nondirectory file)))
         (dir (concat default-directory
                      pjones:markdown-attachments-directory))
         (dest (concat (file-name-as-directory dir)
                       (file-name-nondirectory name))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (copy-file file dest)
    (unless (= 0 (call-process "git-annex" nil nil nil "info"))
      (call-process "git-annex" nil nil nil "init"))
    (unless (= 0 (call-process "git-annex" nil nil nil "add" dest))
      (error "Error: git-annex failed"))
    (markdown-insert-reference-definition
     (file-name-nondirectory name)
     (file-relative-name dest))))

(defhydra hydra-markdown (:hint nil :color blue) "
^Links^                ^Cleanup^            ^Headings and Lists^
-----------------------------------------------------------------
_i_: insert            _r_: renumber        _f_: demote
_j_: jump              _/_: complete        _b_: promote
_c_: check             ^ ^                  _n_: down
_a_: attach            ^ ^                  _p_: up
^ ^                    ^ ^                  _N_: move down
^ ^                    ^ ^                  _P_: move up

^Render^               ^Insert^
-----------------------------------------------------------------
_l_: preview           _; i_: fence insert
_L_: live              _; c_: fence block
^ ^                    _; n_: note
^ ^                    ^   ^
"
  ("i" markdown-insert-link)
  ("j" markdown-do)
  ("c" markdown-check-refs)
  ("a" pjones:markdown-attach-file)
  ("r" markdown-cleanup-list-numbers)
  ("n" markdown-outline-next :color red)
  ("p" markdown-outline-previous :color red)
  ("f" markdown-demote :color red)
  ("b" markdown-promote :color red)
  ("N" markdown-move-down :color red)
  ("P" markdown-move-up :color red)
  ("/" markdown-complete)
  ("l" markdown-preview)
  ("L" markdown-live-preview-mode)
  ("; i" pjones:markdown-slide-fenced-code-insert)
  ("; c" pjones:markdown-slide-fenced-code-block)
  ("; n" pjones:markdown-slide-notes))

(defun pjones:markdown-mode-hook ()
  "Set up key bindings and other crap for markdown-mode."
  (local-set-key (kbd "C-c C-a")    #'pjones:markdown-attach-file)
  (local-set-key (kbd "C-c C-o")    #'pjones:markdown-follow-thing-at-point)
  (local-set-key (kbd "C-c /")      #'markdown-complete)
  (local-set-key (kbd "C-<return>") #'markdown-insert-header-dwim)
  (local-set-key (kbd "M-<return>") #'pjones:markdown-insert-list-item)
  (local-set-key (kbd "C-c C-h")    #'hydra-markdown/body)
  (whitespace-mode)
  (orgalist-mode)
  (orgtbl-mode)
  (pjones:add-fixme-lock)

  ;; Completion configuration:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-ispell
                                   company-dabbrev))

  ;; Files in /tmp that are *.txt are from my browser and most
  ;; websites don't like it when text you submit has newlines.
  (when (and buffer-file-name (string-match "^/tmp/.*\\.txt$" buffer-file-name))
    (pjones:markdown-visual-line)))

(add-hook 'markdown-mode-hook 'pjones:markdown-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; markdown-mode-conf.el ends here
