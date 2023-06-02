;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.
;;
;;; Commentary:
;;
;;; Code:

(require 'savehist)

(declare-function cape-capf-buster "cape")
(declare-function cape-dabbrev "cape")
(declare-function cape-file "cape")
(declare-function cape-keyword "cape")
(declare-function cape-tex "cape")
(declare-function consult-completion-in-region "consult")
(declare-function corfu-history-mode "corfu-history")
(declare-function corfu-insert "corfu")
(declare-function corfu-insert-separator "cofu")
(declare-function corfu-popupinfo-mode "corfu-popupinfo")
(declare-function corfu-popupinfo-scroll-down "corfu-popupinfo")
(declare-function corfu-popupinfo-scroll-up "corfu-popupinfo")
(declare-function global-corfu-mode "corfu")
(declare-function orderless-escapable-split-on-space "orderless")
(declare-function yas-expand "yasnippet")
(declare-function yas-maybe-expand-abbrev-key-filter "yasnippet")

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
(defvar corfu--extra)

(defun pjones:corfu-mode-hook ()
  "Hook for `corfu-mode-hook'."
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (define-key corfu-map (kbd "SPC") #'corfu-insert-separator)
  (define-key corfu-map (kbd "<return>") #'corfu-insert)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
  (define-key corfu-map "\M-m" #'pjones:corfu-move-to-minibuffer))

(defun pjones:corfu-move-to-minibuffer ()
  "Transfer corfu completion to the minibuffer via consult."
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

(defun pjones:orderless-escapable-split-on-space (string)
  "Split STRING via `orderless-escapable-split-on-space' then split on slashes."
  (flatten-tree
   (mapcar (lambda (str)
             (split-string str "[/-]"))
           (orderless-escapable-split-on-space string))))

(defun pjones:orderless-bang-without (pattern _index _total)
  "Negate matches for orderless.
PATTERN is passed to `orderless-without-literal'."
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

(add-hook 'after-init-hook #'global-corfu-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
(add-hook 'corfu-mode-hook #'pjones:corfu-mode-hook)

(setq-default completion-at-point-functions
              (list #'cape-file
                    #'cape-tex
                    #'cape-keyword
                    (cape-capf-buster #'cape-dabbrev)))

(custom-set-variables
 '(dabbrev-case-fold-search nil) ; Don't mess with case.
 '(completion-category-defaults nil)
 '(completion-category-overrides '((file (styles . (partial-completion)))))
 '(completion-styles '(orderless partial-completion basic))
 '(completions-detailed t)
 '(corfu-quit-no-match 'separator)
 '(corfu-scroll-margin 5)
 '(orderless-component-separator #'pjones:orderless-escapable-split-on-space)
 '(orderless-style-dispatchers '(pjones:orderless-bang-without)))

;;; completion.el ends here
