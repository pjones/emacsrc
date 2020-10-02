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

(declare-function pjones:markdown-bind-keys "./make-mode-conf")

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

(defun pjones:neuron-rename-buffer ()
  "Rename neuron buffer to include the title."
  (save-match-data
    (let ((base-name (buffer-name))
          (title (save-excursion
                   (goto-char (point-min))
                   (when (search-forward-regexp "^# ")
                     (buffer-substring-no-properties
                      (point)
                      (progn
                        (end-of-line)
                        (point)))))))

      (and (string-match " (title: .+)\\'" base-name)
           (not (and buffer-file-name
                     (string= base-name
                              (file-name-nondirectory buffer-file-name))))
           ;; If the existing buffer name has a (title: xxxx),
           ;; which isn't part of the file name (if any),
           ;; then get rid of that.
           (setq base-name (substring base-name 0 (match-beginning 0))))
      (rename-buffer
       (generate-new-buffer-name
        (concat base-name (if title (concat " (title: " title ")")))))
      (force-mode-line-update))))

(add-hook 'neuron-mode-hook 'pjones:neuron-bind-keys)
(add-hook 'neuron-mode-hook 'pjones:neuron-rename-buffer)

;; Prevent neuron-mode from messing with my writing buffers.
(remove-hook
 'markdown-mode-hook
 #'neuron--auto-enable-when-in-zettelkasten)

;;; neuron-mode-conf.el ends here
