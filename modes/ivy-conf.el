;;; ivy-conf.el --- Settings for Ivy, Counsel, and Swiper.

;;; Commentary:

;;; Code:
(require 'ivy)
(require 'counsel)
(require 'ivy-posframe)

(custom-set-variables
 ;; Ivy:
 '(ivy-height 15) ; Maximum window height.
 '(ivy-count-format "(%d/%d) ")
 '(ivy-wrap t)
 '(ivy-action-wrap t)
 '(ivy-display-style 'fancy)
 '(ivy-on-del-error-function nil)
 '(ivy-use-virtual-buffers t)
 '(ivy-display-function nil)
 '(ivy-use-selectable-prompt t)
 '(ivy-format-function 'ivy-format-function-default)
 '(ivy-virtual-abbreviate 'abbreviate)

 ;; If either of these two settings are `t' Ivy will break
 ;; minibuffer-only frames.
 '(ivy-fixed-height-minibuffer t)
 '(ivy-add-newline-after-prompt nil)

 ;; Display in a posframe:
 '(ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
 '(ivy-posframe-parameters '((internal-border-width . 2)))

 ;; Counsel:
 '(counsel-find-file-at-point nil) ; Use M-n instead
 '(counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))

;; Some key binding improvements:
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-delete-char)

;; Improve some faces too:
(custom-set-faces
 '(ivy-current-match ((t (:inherit 'isearch)))))

;; Enable complementary modes:
(require 'ivy-rich)
(ivy-rich-mode 1)
(ivy-posframe-mode 1)

(defun pjones:ivy-ignore-buffers (buffer)
  "Ignore BUFFER if it meets certain criteria."
  (let* ((buf  (get-buffer buffer))
         (vars (buffer-local-variables buf)))
    (or (get-buffer-window buf t)
        (eq 'fundamental-mode (alist-get 'major-mode vars)))))

(add-to-list 'ivy-ignore-buffers #'pjones:ivy-ignore-buffers)

;;; ivy-conf.el ends here
