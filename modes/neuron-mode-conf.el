;;; neuron-mode-conf.el -- Settings for `neuron-mode'
;;
;;; Commentary:
;;
;;; Code:
(require 'rg)
(require 'markdown-mode)
(require 'neuron-mode)

(declare-function pjones:markdown-bind-keys "./markdown-conf")

(custom-set-variables
 '(neuron-daily-note-title-format "%A, %B %d, %Y")
 '(neuron-use-short-links nil)
 `(neuron-default-zettelkasten-directory
   ,(expand-file-name "~/notes/zettelkasten/")))

(rg-define-search pjones:zettel-need-to-do
  "Search for zettels that have open check boxes."
  :format regexp
  :query "\\[ \\]"
  :files "*.md"
  :dir (eval (neuron-zettelkasten)))

(rg-define-search pjones:rg-zettel-dir
  "Search through my zettel collection."
  :format regexp
  :files "*.md"
  :dir (eval (neuron-zettelkasten)))

(defun pjones:neuron-bind-keys ()
  "Bind keys in modes derived from `markdown-mode'."
  (pjones:markdown-bind-keys))

(let ((map neuron-mode-map))
  (define-key map (kbd "C-c C-o") #'neuron-follow-thing-at-point)
  (define-key map (kbd "C-c C-i") #'neuron-create-and-insert-zettel-link))

(add-hook 'neuron-mode-hook 'pjones:neuron-bind-keys)

;; Prevent neuron-mode from messing with my writing buffers.
(remove-hook
 'markdown-mode-hook
 #'neuron--auto-enable-when-in-zettelkasten)

;;; neuron-mode-conf.el ends here
