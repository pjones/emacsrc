;;; shackle-conf.el -- Settings for shackle.
(eval-when-compile
  (require 'shackle))

;; https://github.com/wasamasa/shackle

(defun pjones:shackle-split (buffer _alist _plist)
  "Split the current window."
  (let ((window (split-window)))
    (when window (set-window-buffer window buffer))))

(custom-set-variables
 '(shackle-default-rule (quote (:same t)))
 '(shackle-rules nil))

;; Modes that should get their own selected windows:
(add-to-list 'shackle-rules '(help-mode :same nil))
(add-to-list 'shackle-rules '("*JS scratch*" :regex t :same nil))

;; Modes that should get their own windows, but remain inactive:
(add-to-list 'shackle-rules '(compilation-mode :noselect t :same nil))
(add-to-list 'shackle-rules '(grep-mode :noselect t :same nil))
(add-to-list 'shackle-rules '(magit-diff-mode :noselect t :same nil))

;; Windows that should split the entire frame:
(add-to-list 'shackle-rules '("*Completions*" :regexp t :size 0.3 :align 'below :noselect t))
(add-to-list 'shackle-rules '("*Deletions*" :regexp t :size 0.3 :align 'below :noselect t))
(add-to-list 'shackle-rules '(calendar-mode :size 0.2 :align 'below :noselect t))
(add-to-list 'shackle-rules '("\\*magit-.*popup" :same nil :regexp t :custom pjones:shackle-split))

;; PDF Outline windows should always split the current window:
(add-to-list 'shackle-rules
  '(pdf-outline-buffer-mode :same nil :custom pjones:shackle-split))

;;; shackle-conf.el ends here
