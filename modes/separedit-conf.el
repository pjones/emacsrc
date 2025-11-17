;;; separedit-conf.el -- Settings for `separedit' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'separedit)

(custom-set-variables
  '(separedit-default-mode 'sh-mode)
  '(separedit-code-block-default-mode 'sh-mode)
  '(separedit-continue-fill-column t)
  '(separedit-preserve-string-indentation t))

;; Add support for Pandoc Markdown:
(add-to-list 'separedit-block-regexp-plists
             `(:header ,(rx (>= 3 "`")
                            (+ (char blank))
                            (or (seq (group (+ (char word)))
                                     word-boundary)
                                (seq (seq (char ?{) (char ?.))
                                     (group (+ (char word)))
                                     word-boundary)))
               :footer ,(rx (>= 3 "`")
                            line-end)
               :modes (markdown-mode)
               :body   ""))

;; Add support for YAML blocks (CI/CD bullshit):
(add-to-list 'separedit-block-regexp-plists
             `(:header ,(rx bol (= 6 ? ) "run: |")
               :footer ,(rx bol
                            (repeat 0 6 ? )
                            (any alphanumeric punctuation))
               :body ,(rx bol (= 8 ? ))
               :straight t
               :modes (yaml-mode yaml-ts-mode)
               :edit-mode sh-mode))

(add-to-list 'separedit-code-lang-modes
             '("python" . python-mode))

;;; separedit-conf.el ends here
