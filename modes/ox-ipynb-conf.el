;;; ox-ipynb-conf.el -- Settings for `ox-ipynb' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'ox-ipynb)

;; Treat python blocks as ipython blocks so I don't have to have
;; another mode to support stupid ipython blocks:
(dolist (list '(ox-ipynb-kernelspecs ox-ipynb-language-infos))
  (add-to-list list (cons 'python (alist-get 'ipython (symbol-value list)))))

;;; ox-ipynb-conf.el ends here
