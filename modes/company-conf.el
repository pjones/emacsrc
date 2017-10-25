;;; company-conf.el --- company-mode configuration.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'company))

;; Settings for company-mode:
(custom-set-variables
  '(company-show-numbers nil)
  '(company-selection-wrap-around t)
  '(company-lighter-base "")
  '(company-dabbrev-ignore-case nil)
  '(company-dabbrev-downcase nil)
  '(company-backends '((company-capf
                        company-abbrev
                        company-dabbrev
                        company-keywords
                        company-files
                        company-ispell))))

(defun pjones:company-mode-hook ()
  (let ((map company-active-map))
    (define-key map (kbd "C-w")   'pjones:kill-region-or-backward-kill-word)
    (define-key map (kbd "C-f")   'company-complete-selection)
    (define-key map (kbd "C-n")   'company-select-next)
    (define-key map (kbd "C-p")   'company-select-previous)
    (define-key map (kbd "C-s")   'company-filter-candidates)
    (define-key map (kbd "<tab>") 'company-complete-common-or-cycle))

  ;; Enable fuzzy (flx) matching in company-mode.
  (company-flx-mode +1))

(add-hook 'company-mode-hook 'pjones:company-mode-hook)
