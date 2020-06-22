;;; ivy-conf.el --- Settings for Ivy, Counsel, and Swiper.

;;; Commentary:

;;; Code:
(require 'ivy)
(require 'counsel)

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
 '(ivy-fixed-height-minibuffer t)
 '(ivy-add-newline-after-prompt nil)

 ;; Counsel:
 '(counsel-find-file-at-point nil) ; Use M-n instead
 '(counsel-find-file-ignore-regexp "\\(?:\\`\\|[/\\]\\)\\(?:[#.]\\)"))

;; Custom key bindings:
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)

;; Improve some faces too:
;; (custom-set-faces
;;  '(ivy-current-match ((t (:inherit 'isearch)))))

;; Enable complementary modes:
(require 'ivy-rich)
(ivy-rich-mode 1)

(defun pjones:ivy-ignore-buffers (buffer)
  "Ignore BUFFER if it meets certain criteria."
  (let* ((buf  (get-buffer buffer)))
    (get-buffer-window buf t)))

(add-to-list 'ivy-ignore-buffers #'pjones:ivy-ignore-buffers)

;;; ivy-conf.el ends here
