;;; functions.el -- Non-interactive functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(declare-function eww-decode-url-file-name "eww")
(declare-function url-http-head "url-http")
(declare-function url-path-and-query "url-parse")

(defun pjones:frame-popup-p (&optional frame)
  "Return non-nil if FRAME is a popup frame."
  (let ((params (frame-parameters (or frame (selected-frame)))))
    ;; See buffers.el for info about `x-name'.
    (or (string= "popup" (cdr (assq 'x-name params)))
        (string= "popup" (cdr (assq 'name params))))))

(defun pjones:display-buffer-in-non-popup-frame (buffer)
  "Display and select BUFFER for a server client."
  (let* ((not-popup-p (lambda (frame) (not (pjones:frame-popup-p frame))))
         (actions (list :frame-predicate not-popup-p)))
    (if-let ((window (display-buffer-use-some-frame buffer actions)))
        (progn
          (select-frame-set-input-focus (window-frame window))
          (select-window window))
      ;; Fallback.
      (pop-to-buffer buffer))))

(defun pjones:script (name)
  "Generate an absolute path to the script NAME."
  (concat
   (file-name-directory
    (directory-file-name
     (file-name-directory
      (or load-file-name
          byte-compile-current-file
          (buffer-file-name)))))
   "scripts/" name))

(defun pjones:url-file-name (url)
  "Try to get the file name associated with a URL."
  (require 'url-http)
  (require 'url-parse)
  (let* ((uobj (url-generic-parse-url url))
         (upath (car (url-path-and-query uobj)))
         (base (or (file-name-base upath) ""))
         (ext (or (file-name-extension upath) ""))
         (get (lambda (re)
                (save-excursion
                  (if (re-search-forward re nil t)
                      (match-string 1))))))
    (when (or (string-empty-p base)
              (string-empty-p ext))
      (let ((http (url-http-head url))
            hext hname)
        (with-current-buffer http
          (goto-char (point-min))
          (setq hname (funcall get (rx "content-disposition: "
                                       (* anything)
                                       "filename=\""
                                       (group (+ (not ?\")))))
                hext (funcall get (rx "content-type: "
                                      (group (+ (not (any space control)))))))
          (if (and (string-empty-p base) hname)
              (setq base hname))
          (if (and (string-empty-p ext) hext)
              (setq ext (symbol-name
                          (mailcap-mime-type-to-extension
                           hext)))))
        (kill-buffer http)))
     (eww-decode-url-file-name (concat base "." ext))))

;;; functions.el ends here
