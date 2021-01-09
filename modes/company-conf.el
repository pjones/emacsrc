;;; company-conf.el --- company-mode configuration.
;;
;;; Commentary:
;;
;;; Code:
(require 'company)
(require 'company-quickhelp)
(require 'company-statistics)

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
        company-dabbrev-code
        company-keywords
        company-files))))

(let ((map company-active-map))
  (define-key map (kbd "<escape>") #'company-abort)
  (define-key map (kbd "C-f") #'company-abort)
  (define-key map (kbd "C-/") #'company-filter-candidates)
  (define-key map (kbd "C-h") #'company-quickhelp-manual-begin)
  (define-key map (kbd "<tab>") #'company-complete-common-or-cycle)
  (dotimes (i 10)
    (define-key
      map
      (read-kbd-macro (format "C-%d" i))
      'company-complete-number)))

(let ((map company-search-map))
  (define-key map (kbd "<escape>") #'company-abort)
  (define-key map (kbd "C-f") #'company-abort)
  (define-key map (kbd "n") #'company-search-repeat-forward))

(add-hook 'company-mode-hook #'company-quickhelp-mode)
(add-hook 'company-mode-hook #'company-statistics-mode)

;;; company-conf.el ends here
