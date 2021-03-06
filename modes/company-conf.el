;;; company-conf.el --- company-mode configuration.
;;
;;; Commentary:
;;
;;; Code:

(require 'company)
(require 'company-quickhelp)

;; Settings for company-mode:
(custom-set-variables
  '(company-idle-delay nil)
  '(company-show-numbers t)
  '(company-selection-wrap-around t)
  '(company-dabbrev-ignore-case nil)
  '(company-dabbrev-downcase nil)
  '(company-require-match 'never)
  '(company-dabbrev-code-other-buffers 'all)
  '(company-tooltip-align-annotations t)
  '(company-quickhelp-use-propertized-text t)
  '(company-transformers
    '(company-sort-by-occurrence))
  '(company-frontends
    '(company-pseudo-tooltip-frontend
      company-preview-frontend
      company-echo-metadata-frontend))
  '(company-backends
    '((company-capf
       company-keywords
       company-files
       company-dabbrev))))

(let ((map company-active-map))
  (define-key map (kbd "C-s") #'company-filter-candidates)
  (define-key map (kbd "C-h") #'company-quickhelp-manual-begin)
  (define-key map (kbd "<tab>") #'company-complete-common-or-cycle)
  (define-key map (kbd "C-n") #'company-select-next)
  (define-key map (kbd "C-p") #'company-select-previous)
  (dotimes (i 10)
    (define-key
      map
      (read-kbd-macro (format "C-%d" i))
      'company-complete-number)))

(let ((map company-search-map))
  (define-key map (kbd "C-n") #'company-select-next)
  (define-key map (kbd "C-p") #'company-select-previous))

(add-hook 'company-mode-hook #'company-quickhelp-mode)

;;; company-conf.el ends here
