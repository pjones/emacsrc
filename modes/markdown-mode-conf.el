;;; markdown-conf.el -- Settings for markdown-mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'markdown-mode)
  (require 'whitespace)
  (require 'company))

;; Used for a few things.
(require 'org)

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

(defun pjones:markdown-visual-line ()
  "Don't wrap lines.  Needed for most web forms."
  (interactive)
  (setq whitespace-style (delq 'lines-tail whitespace-style))
  (auto-fill-mode -1)
  (visual-line-mode))

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

(defhydra hydra-markdown (:hint nil) "
^Links^                ^Cleanup^                 ^Headings^
-----------------------------------------------------------------------
   _C-c C-l_: insert   _C-c C-c n_: renumber     _C-c C-f_: down
   _C-c C-d_: jump     _C-M-f_: demote           _C-c C-b_: up
 _C-c C-c c_: check    _C-M-b_: promote          _C-c C-n_: down (any)
   _C-c C-a_: attach   _C-c /_: complete         _C-c C-p_: up (any)

^Render^               ^Insert^
-----------------------------------------------------------------------
 _b_: browser          _i_: fence insert
 _l_: live             _c_: fence block
 ^ ^                   _n_: note
"
  ("C-c C-l" markdown-insert-link :color blue)
  ("C-c C-d" markdown-do :color blue)
  ("C-c C-c c" markdown-check-refs :color blue)
  ("C-c C-a" pjones:markdown-attach-file :color blue)
  ("C-c C-c n" markdown-cleanup-list-numbers :color blue)
  ("C-c C-f" markdown-outline-next-same-level :color blue)
  ("C-c C-b" markdown-outline-previous-same-level :color blue)
  ("C-c C-n" markdown-outline-next :color blue)
  ("C-c C-p" markdown-outline-previous :color blue)
  ("C-M-f" markdown-demote :color blue)
  ("C-M-b" markdown-promote :color blue)
  ("C-c /" markdown-complete :color blue)
  ("b" markdown-preview :color blue)
  ("l" markdown-live-preview-mode :color blue)
  ("i" pjones:markdown-slide-fenced-code-insert :color blue)
  ("c" pjones:markdown-slide-fenced-code-block :color blue)
  ("n" pjones:markdown-slide-notes :color blue))

(defun pjones:markdown-mode-hook ()
  "Set up key bindings and other crap for markdown-mode."
  (local-set-key (kbd "C-c C-a") 'pjones:markdown-attach-file)
  (local-set-key (kbd "C-c C-o") 'pjones:markdown-follow-thing-at-point)
  (local-set-key (kbd "C-c /")   'markdown-complete)
  (local-set-key (kbd "C-M-p")   'markdown-move-list-item-up)
  (local-set-key (kbd "C-M-n")   'markdown-move-list-item-down)
  (local-set-key (kbd "C-M-f")   'markdown-demote)
  (local-set-key (kbd "C-M-b")   'markdown-promote)
  (local-set-key (kbd "C-RET")   'markdown-insert-header-dwim)
  (local-set-key (kbd "C-c h")   'hydra-markdown/body)
  (abbrev-mode)
  (whitespace-mode)
  (orgstruct-mode)
  (pjones:add-fixme-lock)

  ;; Completion configuration:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-ispell
                                   company-dabbrev))

  (define-abbrev-table 'markdown-mode-abbrev-table
    '(("fn" "" pjones:markdown-slide-notes)
      ("fc" "" pjones:markdown-slide-fenced-code-block)
      ("fi" "" pjones:markdown-slide-fenced-code-insert)))
  (setq local-abbrev-table markdown-mode-abbrev-table)

  ;; Files in /tmp that are *.txt are from my browser and most
  ;; websites don't like it when text you submit has newlines.
  (when (and buffer-file-name (string-match "^/tmp/.*\\.txt$" buffer-file-name))
    (pjones:markdown-visual-line)))

(add-hook 'markdown-mode-hook 'pjones:markdown-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
