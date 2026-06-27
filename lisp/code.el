;;; code.el -- Settings and functions for programming modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 's)

(declare-function dumb-jump-xref-activate "dumb-jump")
(declare-function indium-connect-to-chrome "indium")
(declare-function indium-run-node "indium")
(declare-function outline-indent-minor-mode "outline-indent")
(declare-function pjones:delete-whitespace-mode "./whitespace.el")
(declare-function pjones:project-name "./functions")
(declare-function project-root "project")
(declare-function puni-mode "puni")
(declare-function s-trim "s")
(declare-function yas-minor-mode "yasnippet")

(defun pjones:comment-bar-str ()
  "Return a comment bar string."
  (let* ((cs (s-trim comment-start))
         (col (current-column))
         (info (cond
                ((string= cs "--") '("-" "-"  ""))
                ((string= cs "//") '("*" "/*" "*/"))
                ((string= cs "/*") '("*" "/*" "*/"))
                ((string= cs "#")  '("#" "#"  "#"))
                (t (list "#" comment-start
                         (if (> (length comment-end) 0)
                             comment-end
                           "")))))
         (spacer (nth 0 info))
         (start (nth 1 info))
         (end   (nth 2 info)))
    (concat start
            (s-repeat (- 80 (length start) (length end) col) spacer)
            end)))

(defun pjones:comment-bar ()
  "Create a comment bar based on the current mode."
  (interactive)
  (let* ((col (current-column))
         (leading (buffer-substring
                   (save-excursion
                     (beginning-of-line)
                     (point))
                   (point))))
    (if (string-match-p "^\\s-*$" leading)
        (progn
          (insert (pjones:comment-bar-str))
          (newline)
          (insert leading))
      (save-excursion
        (back-to-indentation)
        (setq col (current-column))
        (beginning-of-line)
        (open-line 1)
        (insert-char ?  col)
        (insert (pjones:comment-bar-str))))))

(defun pjones:prog-mode-hook ()
  "Settings and bindings for programming modes."
  (keymap-local-set "C-<tab>" #'pjones:comment-bar)

  (setq-local
   comment-auto-fill-only-comments t)   ; Don't auto fill code.

  (auto-fill-mode)
  (flymake-mode)
  (outline-indent-minor-mode)
  (pjones:delete-whitespace-mode)
  (puni-mode)
  (save-place-mode)
  (yas-minor-mode)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 50 t)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p 0 t)

  ;; Some modes (csharp-mode) put `t' in the backend list!
  (setq xref-backend-functions
        (-filter
         (lambda (elm) (not (equal t elm)))
         xref-backend-functions)))

;; Hook In:
(add-hook 'prog-mode-hook #'pjones:prog-mode-hook)

(defun pjones:indium-start-chrome ()
  "Start an Indium session for Chrome."
  (interactive)
  (pjones:indium-start "chrome"))

(defun pjones:indium-start-node ()
  "Start an Indium session for Node.js."
  (interactive)
  (pjones:indium-start "node"))

(defun pjones:indium-start (type)
  "Start an Indium process for TYPE."
  (require 'indium)
  (cond
   ((get-buffer "*JS REPL*")
    (switch-to-buffer "*JS REPL*"))
   ((string= type "chrome")
    (indium-connect-to-chrome))
   ((string= type "node")
    (indium-run-node "node"))))

(defvar pjones:cc-file-extensions
  '(".h" ".hh" ".hpp" ".c" ".cc" ".cpp" ".cxx")
  "List of file extensions for C/C++ file.")

(defun pjones:cc-file-p (file)
  "Return non-nil if FILE is a C/C++ file."
  (member (file-name-extension file t)
          pjones:cc-file-extensions))

(defun pjones:cc-add-extension (paths)
  "Add file extensions to PATHS.
Each path will produce appended with each extension."
  (flatten-list
   (mapcar (lambda (path)
             (mapcar (lambda (ext) (concat path ext))
                     pjones:cc-file-extensions))
           (flatten-list paths))))

(defun pjones:cc-include-to-src (from-root)
  "Translate the path FROM-ROOT from a header to a source file.
The path should be relative to the project root.  Returns all possible
matching paths even if they don't exist.  Does not change file
extensions."
  (let ((re (rx (or ?/ word-boundary)
                (group "include/" (? (+ (not ?/)) ?/)))))
    (when (string-match re from-root)
      (list
       (replace-match "source/" t t from-root 1)
       (replace-match "src/" t t from-root 1)))))

(defun pjones:cc-src-to-include (from-root project-name)
  "Translate the path FROM-ROOT from a source to a header file.
PROJECT-NAME is used to form header paths that include the project name.
The path should be relative to the project root.  Returns all possible
matching paths even if they don't exist.  Does not change file
extensions."
  (let ((src (rx (or ?/ word-boundary) (group "src/")))
        (source (rx (or ?/ word-boundary) (group "source/"))))
    (when (or (string-match source from-root)
              (string-match src from-root))
      (list
       (replace-match "include/" t t from-root 1)
       (replace-match (concat "include/" project-name "/") t t from-root 1)))))

(defun pjones:find-sibling-cc (file)
  "Return a C/C++ sibling file for FILE."
  (when-let*
      (((pjones:cc-file-p file))
       (sans-ext (file-name-sans-extension (expand-file-name file)))
       (project (project-current t))
       (root (project-root project))
       (project-name (pjones:project-name))
       (from-root (file-relative-name sans-ext root))
       (base (file-name-base file))
       (paths (list from-root
                    (pjones:cc-include-to-src from-root)
                    (pjones:cc-src-to-include from-root project-name)
                    (concat "test/" base "_test")
                    (concat "test/" "test_" base))))
    (seq-filter #'file-exists-p
                (mapcar (apply-partially #'concat root)
                        (pjones:cc-add-extension paths)))))

;; Add rules for finding sibling files:
(add-to-list 'find-sibling-rules #'pjones:find-sibling-cc)

(defun pjones:find-sibling-file-search (file &optional rules)
  "Return a list of FILE's \"siblings\".
RULES should be a list on the form defined by `find-sibling-rules' (which
see), and if nil, defaults to `find-sibling-rules'."
  (let ((results nil))
    ;;(pcase-dolist (`(,match . ,expansions) (or rules find-sibling-rules))
    (dolist (rule (or rules find-sibling-rules))
      (pcase rule
        (`(,match . ,expansions)
         ;; Go through the list and find matches.
         (when (string-match match file)
           (let ((match-data (match-data)))
             (dolist (expansion expansions)
               (let ((start 0))
                 ;; Expand \\1 forms in the expansions.
                 (while (string-match "\\\\\\([&0-9]+\\)" expansion start)
                   (let ((index (string-to-number (match-string 1 expansion))))
                     (setq start (match-end 0)
                           expansion
                           (replace-match
                            (substring file
                                       (elt match-data (* index 2))
                                       (elt match-data (1+ (* index 2))))
                            t t expansion)))))
               ;; Then see which files we have that are matching.  (And
               ;; expand from the end of the file's match, since we might
               ;; be doing a relative match.)
               (let ((default-directory (substring file 0 (car match-data))))
                 ;; Keep the first matches first.
                 (setq results
                       (nconc
                        results
                        (mapcar #'expand-file-name
                                (file-expand-wildcards expansion nil t)))))))))
        ((pred functionp)
         (setq results
               (nconc results
                      (mapcar #'expand-file-name (funcall rule file)))))))
    ;; Delete the file itself (in case it matched), and remove
    ;; duplicates, in case we have several expansions and some match
    ;; the same subsets of files.
    (delete file (delete-dups results))))

(advice-add 'find-sibling-file-search :override #'pjones:find-sibling-file-search)

;;; code.el ends here
