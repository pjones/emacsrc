;;; isearch-conf.el -- Settings for `isearch' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'isearch)

(declare-function avy-isearch "avy")

(custom-set-variables
 '(isearch-allow-motion t)
 '(isearch-lax-whitespace t)
 '(isearch-lazy-count t)
 '(search-nonincremental-instead nil)
 '(search-whitespace-regexp "[ \t\n.?!]+"))

(keymap-set isearch-mode-map "C-c" #'isearch-abort)
(keymap-set isearch-mode-map "C-g" #'isearch-cancel)
(keymap-set isearch-mode-map "M-g a" #'avy-isearch)
(keymap-set isearch-mode-map "M-g M-a" #'avy-isearch)
(keymap-set isearch-mode-map "n" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "p" #'isearch-repeat-backward)

(defvar-keymap isearch-repeat-map
  :repeat t
  "n" #'isearch-repeat-forward
  "p" #'isearch-repeat-backward
  "r" #'isearch-repeat-backward
  "s" #'isearch-repeat-forward)

;;; isearch-conf.el ends here
