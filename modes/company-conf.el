;;; company-conf.el --- company-mode configuration.
;;
;;; Commentary:
;;
;;; Code:
(require 'company)
(require 'company-quickhelp)
(require 'company-statistics)

(declare-function pjones:kill-region-or-backward-kill-word "../lisp/interactive.el")

;; Settings for company-mode:
(custom-set-variables
  '(company-idle-delay nil)
  '(company-show-numbers t)
  '(company-selection-wrap-around t)
  '(company-dabbrev-ignore-case nil)
  '(company-dabbrev-downcase nil)
  '(company-dabbrev-code-other-buffers t)
  '(company-tooltip-align-annotations t)
  '(company-quickhelp-use-propertized-text t)
  '(company-transformers '(company-sort-by-occurrence))
  '(company-frontends '(company-pseudo-tooltip-frontend
                        company-preview-frontend
                        company-echo-metadata-frontend)))

(defun pjones:company-mode-hook ()
  "Peter's mode hook for company-mode."
  (let ((map company-active-map))
    (define-key map (kbd "<escape>") #'company-abort)
    (define-key map (kbd "C-f")      #'company-abort)
    (define-key map (kbd "C-w")      #'pjones:kill-region-or-backward-kill-word)
    (define-key map (kbd "C-s")      #'company-filter-candidates)
    (define-key map (kbd "/")        #'company-filter-candidates)
    (define-key map (kbd "M-j")      #'company-select-next)
    (define-key map (kbd "M-k")      #'company-select-previous)
    (define-key map (kbd "M-h")      #'company-quickhelp-manual-begin)
    (define-key map (kbd "<tab>")    #'company-complete-common-or-cycle))

  (let ((map company-search-map))
    (define-key map (kbd "<escape>") #'company-abort)
    (define-key map (kbd "C-f")      #'company-abort)
    (define-key map (kbd "M-j")      #'company-select-next)
    (define-key map (kbd "M-k")      #'company-select-previous))

  (company-quickhelp-mode)
  (company-statistics-mode))

(add-hook 'company-mode-hook 'pjones:company-mode-hook)

;;; company-conf.el ends here
