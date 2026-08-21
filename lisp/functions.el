;;; functions.el -- Non-interactive functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(declare-function eww-decode-url-file-name "eww")
(declare-function json-read-file "json")
(declare-function project-name "project")
(declare-function project-root "project")
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

;; Allow setting a project name in .dir-local.el.
(defvar-local pjones:project-name nil
  "The name of the current project, as it appears in the \"include\" directory.")

(defun pjones:project-name ()
  "Return the name of the current project."
  (if (and (boundp 'pjones:project-name)
           pjones:project-name)
      pjones:project-name
    (let* ((project (project-current t))
           (root (project-root project))
           (name (project-name project))
           (include-dir (concat root "include/"))
           (include (when (file-exists-p include-dir)
                      (car (seq-filter
                            (lambda (f) (file-directory-p (concat include-dir f)))
                            (directory-files include-dir nil (rx bol (not ?.))))))))
      (or include name))))

(defun pjones:project-license-notice ()
  "Return a license notice for the current project."
  (let* ((indent "") ; For future use.
         (str (concat "This file is part of the "
                      (pjones:project-name)
                      " project.  It is subject to the license specified in"
                      " the LICENSE file which can be found in the top-level"
                      " directory of this repository.")))
    (replace-regexp-in-string (rx bol)
                              indent
                              (string-fill str (- fill-column (length indent))))))

(defun pjones:random-json-entry (file-name &optional key)
  "Return a random entry from FILE-NAME.

Since JSON documents are typically objects, KEY is used to find the list
of quotes.  It defaults to \='quote."
  (require 'json)
  (when-let* ((json (json-read-file (expand-file-name file-name)))
              (key (or key 'quotes)))
    (seq-random-elt (cdr (assoc key json)))))

;;; functions.el ends here
