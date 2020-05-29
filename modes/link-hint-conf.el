;;; link-hint-conf.el -- Settings for `link-hint'
;;
;;; Commentary:
;;
;;; Code:
(require 'link-hint)

(defvar link-hint-avy-all-windows)
(setq link-hint-avy-all-windows t)

(defun pjones:link-hint--action (action link)
  "Take ACTION on LINK.
Links always open in the selected window regardless of which window
contains the link."
  (let* (link-buffer
         link-buffer-original-pos
         (link-pos (plist-get link :pos))
         (link-win (plist-get link :win))
         new-win-buffer
         (type (plist-get link :type))
         (parser (get type :parse))
         (args (plist-get link :args))
         (link-description (link-hint--apply
                            (or (get type :describe) #'identity)
                            args
                            parser
                            :describe))
         ret)
    (setq ret (link-hint--apply (get type action) args parser action))
    (link-hint--message action link-description type)
    ret))

(defun pjones:link-hint-open-link (&optional copy)
  "Use avy to open a visible link in the current window.
If COPY is non-nil, copy the link instead of opening it."
  (interactive "P")
  (avy-with link-hint-open-link
    (let* ((action (if copy :copy :open))
           (link-hint-types (link-hint--valid-types action))
           (links (link-hint--get-links))
           link)
      (when links
        (setq link (link-hint--process links))
        (when link
          (pjones:link-hint--action action link))))))

;;; link-hint-conf.el ends here
