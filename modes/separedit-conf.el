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

(add-to-list 'separedit-code-lang-modes
             '("python" . python-mode))

;;; separedit-conf.el ends here
