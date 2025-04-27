;;; isearch-conf.el -- Settings for `isearch' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'isearch)

(custom-set-variables
 '(isearch-allow-motion t)
 '(isearch-lax-whitespace t)
 '(isearch-lazy-count t)
 '(search-nonincremental-instead nil)
 '(search-whitespace-regexp "[ \t\n.?!]+"))

(defvar-keymap isearch-repeat-map
  :repeat t
  "r" #'isearch-repeat-backward
  "s" #'isearch-repeat-forward)

;;; isearch-conf.el ends here
