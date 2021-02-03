;;; company-conf.el --- company-mode configuration.
;;
;;; Commentary:
;;
;;; Code:

(require 'company)
(require 'company-prescient)
(require 'company-quickhelp)
(require 'company-try-hard)

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
     '(company-capf
       (company-dabbrev
        company-keywords
        company-files
        company-ispell))))

(let ((map company-active-map))
  (define-key map (kbd "<escape>") #'company-abort)
  (define-key map (kbd "C-s") #'company-filter-candidates)
  (define-key map (kbd "C-h") #'company-quickhelp-manual-begin)
  (define-key map (kbd "C-n") #'company-try-hard)
  (define-key map (kbd "<tab>") #'company-complete-common-or-cycle)
  (dotimes (i 10)
    (define-key
      map
      (read-kbd-macro (format "C-%d" i))
      'company-complete-number)))

(let ((map company-search-map))
  (define-key map (kbd "<escape>") #'company-abort)
  (define-key map (kbd "C-n") #'company-try-hard))

(add-hook 'company-mode-hook #'company-quickhelp-mode)
(add-hook 'company-mode-hook #'company-prescient-mode)

;;; company-conf.el ends here
