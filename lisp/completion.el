;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.
;;
;;; Commentary:
;;
;;; Code:

(declare-function cape-capf-buster "cape")
(declare-function cape-dabbrev "cape")
(declare-function cape-file "cape")
(declare-function cape-keyword "cape")
(declare-function cape-tex "cape")
(declare-function corfu-doc-mode "corfu-doc")
(declare-function corfu-doc-scroll-down "corfu-doc")
(declare-function corfu-doc-scroll-up "corfu-doc")
(declare-function corfu-global-mode "corfu")
(declare-function yas-expand "yasnippet")
(declare-function yas-maybe-expand-abbrev-key-filter "yasnippet")

;; I don't want to type "yes".
(defalias 'yes-or-no-p 'y-or-n-p)

;; What to do with the tab key.
(defun pjones:indent-or-complete (&optional arg)
  "Indent or complete.
If the character before point is a space character then indent the
current line.  Otherwise run the completion command.  ARG is passed to
`indent-for-tab-command'."
  (interactive "P")
  (if (region-active-p) (indent-region (region-beginning) (region-end))
    (let ((tab-always-indent t)
          (max-backtrack (save-excursion (beginning-of-line) (point))))
      (cond
       ;; Force indenting the entire expression:
       (arg
        (indent-for-tab-command arg))
       ;; Maybe indent the line:
       ((or (bolp) (looking-back "\\s-" max-backtrack))
        (indent-for-tab-command arg))
       ;; Try snippet completion:
       ((yas-maybe-expand-abbrev-key-filter t)
        (yas-expand))
       ;; Fallback to completion:
       (t
        (completion-at-point))))))

;; In buffer completion:
(defvar corfu-map)

(defun pjones:corfu-mode-hook ()
  "Hook for `corfu-mode-hook'."
  (define-key corfu-map (kbd "<return>") #'corfu-insert)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up))

(add-hook 'after-init-hook #'corfu-global-mode)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)
(add-hook 'corfu-mode-hook #'pjones:corfu-mode-hook)

(setq-default completion-at-point-functions
              (list #'cape-file
                    #'cape-tex
                    #'cape-keyword
                    (cape-capf-buster #'cape-dabbrev)))

(custom-set-variables
 '(corfu-scroll-margin 5)
 '(completion-styles '(orderless partial-completion))
 '(orderless-component-separator #'orderless-escapable-split-on-space)
 '(completion-category-defaults nil)
 '(completion-category-overrides '((file (styles . (partial-completion))))))

;;; completion.el ends here
