;;; neuron-mode-conf.el -- Settings for `neuron-mode'
;;
;;; Commentary:
;;
;;; Code:
(require 'rg)
(require 'evil)
(require 'evil-leader)
(require 'markdown-mode)
(require 'neuron-mode)

(custom-set-variables
 '(neuron-daily-note-title-format "%A, %B %d, %Y")
 `(neuron-default-zettelkasten-directory
   ,(expand-file-name "~/notes/zettelkasten/")))

(defun pjones:zettel-open-inbox ()
  "Open the inbox.md zettel."
  (interactive)
  (find-file (concat neuron-default-zettelkasten-directory "inbox.md")))

(rg-define-search pjones:zettel-need-to-do
  "Search for zettels that have open check boxes."
  :format regexp
  :query "\\[ \\]"
  :files "*.md"
  :dir (eval (neuron-zettelkasten)))

(rg-define-search pjones:rg-zettel-dir
  "Search through my zettel collection."
  :format regexp
  :files current
  :dir (eval (neuron-zettelkasten)))

(defun pjones:neuron-bind-keys ()
  "Bind keys in modes derived from `markdown-mode'."
  (evil-set-initial-state 'neuron-mode 'normal)
  (pjones:markdown-bind-keys)
  (evil-define-key 'normal neuron-mode-map
    "gx" #'neuron-follow-thing-at-point
    "gr" #'neuron-refresh-buffer)
  (evil-leader/set-key-for-mode 'neuron-mode
    "m e" #'neuron-edit-zettel
    "m i" #'neuron-create-and-insert-zettel-link
    "m o" #'neuron-open-current-zettel
    "m q" #'neuron-query-tags
    "m r" #'neuron-rib-generate
    "m s" #'neuron-insert-static-link
    "m t" #'neuron-add-tag
    "m w" #'neuron-rib-watch
    "m W" #'neuron-rib-serve))

(add-hook 'neuron-mode-hook 'pjones:neuron-bind-keys)

;;; neuron-mode-conf.el ends here
