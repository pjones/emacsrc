;;; pdf-tools-conf.el -- Setting for pdf-tools.
;;
;;; Commentary:
;;
;;; Code:
(require 'pdf-tools)
(require 'pdf-outline)
(require 'evil)

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(defun pjones:pdf-outline-follow-link-and-quit ()
  "Follow outline link and delete the outline window."
  (interactive)
  (let ((win (selected-window))
        (buf (current-buffer)))
    (pdf-outline-follow-link nil)
    (delete-window win)
    (kill-buffer buf)))

(defun pjones:pdf-outline-quit ()
  "Close the outline buffer and window."
  (interactive)
  (let ((win (selected-window))
        (buf (current-buffer)))
    (delete-window win)
    (kill-buffer buf)))

(defun pjones:pdf-view-after-change-page-hook ()
  "Respond to a page change in PDF documents."
  (blink-cursor-mode -1)
  (setq-local cursor-type nil)
  (setq-local evil-force-cursor nil)
  (setq-local evil-default-cursor nil)
  (setq-local evil-normal-state-cursor nil)
  (setq-local evil-visual-state-cursor nil)
  (setq-local evil-motion-state-cursor nil)
  (setq-local evil-operator-state-cursor nil)
  (setq-local evil-replace-state-cursor nil)
  (setq-local evil-emacs-state-cursor nil)
  (setq-local evil-insert-state-cursor nil))

;; Settings:
(custom-set-variables
 '(pdf-view-continuous nil))

(defun pjones:pdf-view-mode-hook ()
  "Hook for `pdf-view-mode-hook'."
  (pdf-view-fit-page-to-window)
  (pdf-view-midnight-minor-mode)
  (pjones:pdf-view-after-change-page-hook))

(pjones:evil-override-mode pdf-view-mode
  "j" #'pdf-view-scroll-up-or-next-page
  "k" #'pdf-view-scroll-down-or-previous-page
  "gj" #'pdf-view-next-page-command
  "gk" #'pdf-view-previous-page-command
  "gg" #'pdf-view-first-page
  "G" #'pdf-view-goto-page
  "/" #'isearch-forward
  "?" #'isearch-backward
  "n" #'isearch-repeat-forward
  "N" #'isearch-repeat-backward
  (kbd "<tab>") #'pdf-outline)

(pjones:evil-override-mode pdf-outline-buffer-mode)


;; Extra key bindings:
(define-key pdf-outline-buffer-mode-map (kbd "RET") #'pjones:pdf-outline-follow-link-and-quit)
(define-key pdf-outline-buffer-mode-map (kbd "q")   #'pjones:pdf-outline-quit)

;; Hooks:
(add-hook 'pdf-view-after-change-page-hook #'pjones:pdf-view-after-change-page-hook)
(add-hook 'pdf-view-mode-hook #'pjones:pdf-view-mode-hook)

;;; pdf-tools-conf.el ends here
