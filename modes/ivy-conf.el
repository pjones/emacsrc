;;; ivy-conf.el --- Settings for Ivy, Counsel, and Swiper.

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'ivy)
  (require 'ivy-exwm))

(custom-set-variables
 ;; Ivy:
 '(ivy-height 5)
 '(ivy-fixed-height-minibuffer t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-wrap t)
 '(ivy-action-wrap t)
 '(ivy-display-style 'fancy)
 '(ivy-on-del-error-function nil)
 '(ivy-extra-directories nil)
 '(ivy-use-virtual-buffers t)
 '(ivy-display-function nil)
 '(ivy-use-selectable-prompt t)
 '(ivy-format-function 'ivy-format-function-arrow)
 '(ivy-virtual-abbreviate 'abbreviate)

 ;; Counsel:
 '(counsel-find-file-at-point nil) ; Use M-n instead
 '(counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))

;; Some key binding improvements:
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-delete-char)

;; Enable complementary modes:
(ivy-exwm-mode 1)

;;; ivy-conf.el ends here
