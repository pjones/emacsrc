;;; markdown-conf.el -- Settings for markdown-mode.
(eval-when-compile
  (require 'markdown-mode)
  (require 'org)
  (require 'org-table))

;; Silence a compiler warning.
(declare-function orgtbl-mode "org-table")

;; Basic settings.
(setq markdown-command "pandoc -f markdown -t html"
      markdown-follow-wiki-link-on-enter nil)

(defun pjones:markdown-mode-hook ()
  "Set up key bindings and other crap for markdown-mode."
  (local-set-key (kbd "C-c C-o") 'markdown-follow-link-at-point)
  (whitespace-mode)
  (orgstruct-mode)
  (orgtbl-mode)

  ;; Files in /tmp that are *.txt are from my browser and most
  ;; websites don't like it when text you submit has newlines.
  (when (and buffer-file-name (string-match "^/tmp/.*\\.txt$" buffer-file-name))
    (whitespace-mode -1)
    (auto-fill-mode -1)
    (visual-line-mode)))

(add-hook 'markdown-mode-hook 'pjones:markdown-mode-hook)
