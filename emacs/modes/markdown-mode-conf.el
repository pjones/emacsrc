;;; markdown-conf.el -- Settings for markdown-mode.
(eval-when-compile (require 'markdown-mode))

(setq markdown-command "pandoc -f markdown -t html"
      markdown-follow-wiki-link-on-enter nil)

(defun pjones:markdown-mode-hook ()
  "Set up key bindings and other crap for markdown-mode."
  (local-set-key (kbd "C-c C-o") 'markdown-follow-link-at-point))

(add-hook 'markdown-mode-hook 'whitespace-mode)
(add-hook 'markdown-mode-hook 'pjones:markdown-mode-hook)
