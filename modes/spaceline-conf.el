;;; spaceline-conf.el -- Settings for spaceline.
;;
;;; Commentary:
;;
;;; Code:
(require 'spaceline)
(require 'spaceline-config)
(require 's)

(spaceline-define-segment pjones-buffer-id
  "A better version of buffer-id."
  (let ((face
         (cond
          ((null buffer-file-name) nil)
          (buffer-read-only        'spaceline-read-only)
          ((buffer-modified-p)     'spaceline-modified)
          (t                       'mode-line-buffer-id))))
    (s-trim (spaceline--string-trim-from-center
             (powerline-buffer-id (if active face 'mode-line-buffer-id-inactive))
             spaceline-buffer-id-max-length))))

;; Settings:
(custom-set-variables
 '(spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
 '(spaceline-responsive nil)
 '(spaceline-minor-modes-p nil) ; I don't want to see minor modes.
 '(powerline-default-separator 'arrow))

;; Patch this segment so it actually works.
(spaceline-define-segment erc-track
  "Show the ERC buffers with new messages. Requires
`erc-track-mode' to be enabled and
`erc-track-position-in-mode-line' to be set to true."
  (when (and (bound-and-true-p erc-track-mode)
             erc-track-position-in-mode-line
             erc-modified-channels-object)
    (s-trim erc-modified-channels-object))
  :global-override erc-modified-channels-object)

;; Compile and set the theme:
(spaceline--theme '((evil-state) :face highlight-face :priority 100)
                  '((buffer-modified buffer-size pjones-buffer-id remote-host)))

;;; spaceline-conf.el ends here
