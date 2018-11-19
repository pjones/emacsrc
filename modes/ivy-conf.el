;;; ivy-conf.el --- Settings for Ivy, Counsel, and Swiper.

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'ivy))

(custom-set-variables
 ;; Ivy:
 '(ivy-height 15) ; Maximum window height.
 '(ivy-count-format "(%d/%d) ")
 '(ivy-wrap t)
 '(ivy-action-wrap t)
 '(ivy-display-style 'fancy)
 '(ivy-on-del-error-function nil)
 '(ivy-extra-directories nil)
 '(ivy-use-virtual-buffers t)
 '(ivy-display-function nil)
 '(ivy-use-selectable-prompt t)
 '(ivy-format-function 'ivy-format-function-default)
 '(ivy-virtual-abbreviate 'abbreviate)

 ;; If either of these two settings are `t' Ivy will break
 ;; minibuffer-only frames.
 '(ivy-fixed-height-minibuffer t)
 '(ivy-add-newline-after-prompt nil)

 ;; Counsel:
 '(counsel-find-file-at-point nil) ; Use M-n instead
 '(counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))

;; Some key binding improvements:
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-delete-char)

;; Improve some faces too:
(custom-set-faces
 '(ivy-current-match ((t (:inherit 'isearch)))))

;; Enable complementary modes:
(require 'ivy-exwm)
(ivy-exwm-mode 1)

;;; ivy-conf.el ends here
