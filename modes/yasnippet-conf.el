;;; yasnippet-conf.el -- Settings for yasnippet
;;
;;; Commentary:
;;
;;; Code:
(require 'yasnippet)

(defvar pjones:snippets-dir
  (concat
   (file-name-directory
    (directory-file-name
     (file-name-directory load-file-name))) "snippets/")
  "The directory where I keep snippet files.")

(defun pjones:yas-abort-snippet ()
  "Clear all remaining fields and abort."
  (interactive)
  (yas-clear-field)
  (if (yas-next-field-will-exit-p 1)
      (yas-abort-snippet)
    (yas-next-field 1)
    (pjones:yas-abort-snippet)))

;; I already have a key that triggers yas-expand.
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Arguably better key bindings.
(let ((map yas-keymap))
  ;; Keep TAB for my existing completion key:
  (define-key map [(tab)] nil)
  (define-key map (kbd "TAB")  nil)
  (define-key map [(shift tab)] nil)
  (define-key map [backtab] nil)
  (define-key map (kbd "C-<tab>") #'yas-next-field)
  (define-key map (kbd "M-<tab>") #'yas-prev-field)

  ;; A better version of abort:
  (define-key map (kbd "<escape>") #'pjones:yas-abort-snippet))

(add-to-list 'yas-snippet-dirs pjones:snippets-dir)
(yas-reload-all)

;;; yasnippet-conf.el ends here
