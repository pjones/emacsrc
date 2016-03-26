;;; markdown-conf.el -- Settings for markdown-mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'markdown-mode)
  (require 'whitespace)
  (require 'company))

;; Basic settings.
(setq markdown-command "pandoc -f markdown -t html")

(defvar pjones:markdown-langs
  '("ruby" "javascript" "html" "css")
  "List of pandoc languages I use.")

(define-skeleton pjones:markdown-slide-notes
  "Add a div section for slide notes." nil
  "<div class=\"notes\">"
  ?\n ?\n "  * " _
  ?\n ?\n "</div>")

(defun pjones:markdown-slide-fenced-code-block (&optional with-insert)
  "Insert a markdown fenced code block."
  (interactive "P")
  (let* ((indent (- (point) (save-excursion (forward-line 0) (point))))
         (prefix (make-string indent ? ))
         (lang (ido-completing-read "Lang: " pjones:markdown-langs)))
    (insert (concat "~~~ {." lang "}\n" prefix "\n" prefix "~~~\n"))
    (forward-line -2)
    (end-of-line)
    t))
(put 'pjones:markdown-slide-fenced-code-block 'no-self-insert t)

(defun pjones:markdown-slide-fenced-code-insert nil
  (pjones:markdown-slide-fenced-code-block t))
(put 'pjones:markdown-slide-fenced-code-insert 'no-self-insert t)

(defun pjones:markdown-visual-line ()
  "Don't wrap lines.  Needed for most web forms."
  (interactive)
  (setq whitespace-style (delq 'lines-tail whitespace-style))
  (auto-fill-mode -1)
  (visual-line-mode))

(defun pjones:markdown-mode-hook ()
  "Set up key bindings and other crap for markdown-mode."
  (local-set-key (kbd "C-c C-o") 'markdown-follow-link-at-point)
  (abbrev-mode)
  (whitespace-mode)
  (orgstruct-mode)
  (pjones:add-fixme-lock)

  ;; Completion configuration:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-ispell
                                   company-dabbrev))

  (define-abbrev-table 'markdown-mode-abbrev-table
    '(("nt" "" pjones:markdown-slide-notes)
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
