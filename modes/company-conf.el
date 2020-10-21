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
  '(company-transformers '(company-sort-by-occurrence))
  '(company-frontends '(company-pseudo-tooltip-frontend
                        company-preview-frontend
                        company-echo-metadata-frontend))
  '(company-backends '(company-capf
                       company-files
                       (company-dabbrev-code
                        company-etags
                        company-keywords)
                       company-dabbrev)))

(defun pjones:company-mode-hook ()
  "Peter's mode hook for company-mode."
  (let ((map company-active-map))
    (define-key map (kbd "<escape>") #'company-abort)
    (define-key map (kbd "C-f") #'company-abort)
    (define-key map (kbd "C-/") #'company-filter-candidates)
    (define-key map (kbd "C-h") #'company-quickhelp-manual-begin)
    (define-key map (kbd "<tab>") #'company-complete-common-or-cycle))

  (let ((map company-search-map))
    (define-key map (kbd "<escape>") #'company-abort)
    (define-key map (kbd "C-f") #'company-abort)
    (define-key map (kbd "n") #'company-search-repeat-forward))

  (company-quickhelp-mode)
  (company-statistics-mode))

(add-hook 'company-mode-hook 'pjones:company-mode-hook)

;;; company-conf.el ends here
