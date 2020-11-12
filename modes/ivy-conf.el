;;; ivy-conf.el --- Settings for Ivy, Counsel, and Swiper.

;;; Commentary:

;;; Code:
(require 'counsel)
(require 'evil)
(require 'ivy)

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

(evil-define-key 'insert ivy-minibuffer-map
  (kbd "TAB") #'ivy-next-line
  (kbd "<backspace>") #'ivy-backward-delete-char
  (kbd "<escape>") #'minibuffer-keyboard-quit)

(evil-define-key 'insert counsel-find-file-map
  (kbd "C-w") #'counsel-up-directory)

;; Enable complementary modes:
(require 'ivy-rich)
(ivy-rich-mode 1)

(defun pjones:ivy-ignore-buffers (buffer)
  "Ignore BUFFER if it meets certain criteria."
  (let* ((buf  (get-buffer buffer)))
    (get-buffer-window buf t)))

(add-to-list 'ivy-ignore-buffers #'pjones:ivy-ignore-buffers)

;;; ivy-conf.el ends here
