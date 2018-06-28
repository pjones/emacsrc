;;; shackle-conf.el -- Settings for shackle.
(eval-when-compile
  (require 'shackle))

;; https://github.com/wasamasa/shackle

(defun pjones:shackle-split (buffer _alist _plist)
  "Split the current window."
  (let ((window (split-window)))
    (when window (set-window-buffer window buffer))))

(custom-set-variables
 '(shackle-default-rule (quote (:select t)))
 '(shackle-rules nil))

;; Helm windows should display above the echo area:
(add-to-list 'shackle-rules
  '("\\`\\*helm.*?\\*\\'" :regexp t :align below :size 0.33))

;; Compilation buffers get a new window:
(add-to-list 'shackle-rules '(compilation-mode :noselect t))

;; Man buffers should appear in the current window:
(add-to-list 'shackle-rules '("\\`\\*Man " :regexp t :same t))

;; When asking for completions:
(add-to-list 'shackle-rules '("*Completions*" :size 0.3 :align t))

(add-to-list 'shackle-rules '(calendar-mode :size 0.2 :align below))

;; Don't select grep buffers:
(add-to-list 'shackle-rules '(grep-mode :noselect t))

;; Circe buffers shouldn't split the frame:
(add-to-list 'shackle-rules '(circe-mode :same t))
(add-to-list 'shackle-rules '(circe-server-mode :same t))
(add-to-list 'shackle-rules '(circe-chat-mode :same t))
(add-to-list 'shackle-rules '(circe-channel-mode :same t))

;; PDF Outline windows should always split the current window:
(add-to-list 'shackle-rules
  '(pdf-outline-buffer-mode :custom pjones:shackle-split))

;; Rules for Magit buffers:
(add-to-list 'shackle-rules '(magit-status-mode :same t))
(add-to-list 'shackle-rules '(magit-diff-mode :noselect t))
(add-to-list 'shackle-rules
  '("\\*magit-.*popup" :regexp t :custom pjones:shackle-split))

;; Async shell command buffers:
(add-to-list 'shackle-rules '("\\`\\*Async" :regex t :same t))

;;; shackle-conf.el ends here
