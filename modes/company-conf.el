;;; company-conf.el --- company-mode configuration.
(eval-when-compile
  (require 'company)
  (require 'company-quickhelp)
  (require 'company-statistics))

;; Settings for company-mode:
(custom-set-variables
  '(company-idle-delay 0.25)
  '(company-show-numbers nil)
  '(company-selection-wrap-around t)
  '(company-lighter-base "")
  '(company-dabbrev-ignore-case nil)
  '(company-dabbrev-downcase nil)
  '(company-dabbrev-code-other-buffers t)
  '(company-tooltip-align-annotations t)
  '(company-require-match 'never)
  '(company-quickhelp-use-propertized-text t)
  '(company-transformers '(company-sort-by-occurrence))
  '(company-frontends '(company-pseudo-tooltip-frontend
                        company-echo-metadata-frontend))
  '(company-backends '((company-capf
                        company-abbrev
                        company-dabbrev
                        company-keywords
                        company-files
                        company-ispell))))

(defun pjones:company-mode-hook ()
  "Peter's mode hook for company-mode."
  (let ((map company-active-map))
    (define-key map (kbd "C-w")   #'pjones:kill-region-or-backward-kill-word)
    (define-key map (kbd "C-f")   #'company-complete-selection)
    (define-key map (kbd "C-n")   #'company-select-next)
    (define-key map (kbd "C-p")   #'company-select-previous)
    (define-key map (kbd "C-s")   #'company-filter-candidates)
    (define-key map (kbd "M-h")   #'company-quickhelp-manual-begin)
    (define-key map (kbd "<tab>") #'company-complete-common-or-cycle))

  (company-quickhelp-mode +1)
  (company-statistics-mode))

(add-hook 'company-mode-hook 'pjones:company-mode-hook)
