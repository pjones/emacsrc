;;; isearch-conf.el -- Settings for `isearch' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'isearch)

(custom-set-variables
 '(search-whitespace-regexp ".*")
 '(isearch-lax-whitespace t)
 '(search-nonincremental-instead nil)
 '(isearch-lazy-count t))

;;; isearch-conf.el ends here
