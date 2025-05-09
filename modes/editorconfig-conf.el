;;; editorconfig-conf.el -- Settings for `editorconfig' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'editorconfig)

;; 2025-04-27: Fix a bug where editorconfig will call
;; (file-name-directory) with nil.
(defun editorconfig--get-dir-local-variables ()
  "Return the directory local variables specified via EditorConfig.
Meant to be used on `hack-dir-local-get-variables-functions'."
  (when (stringp buffer-file-name)
    (let* ((props (editorconfig-call-get-properties-function buffer-file-name))
           (alist (editorconfig--get-local-variables props)))
      ;; FIXME: If there's `/foo/.editorconfig', `/foo/bar/.dir-locals.el',
      ;; and `/foo/bar/baz/.editorconfig', it would be nice to return two
      ;; pairs here, so that hack-dir-local can give different priorities
      ;; to the `/foo/.editorconfig' settings compared to those of
      ;; `/foo/bar/baz/.editorconfig', but we can't just convert the
      ;; settings from each file individually and let hack-dir-local merge
      ;; them because hack-dir-local doesn't have the notion of "unset",
      ;; and because the conversion of `indent_size' depends on `tab_width'.
      (when-let* ((alist)
                  (ecf (or buffer-file-name
                           (editorconfig-core-get-nearest-editorconfig buffer-file-name))))
        (cons (file-name-directory ecf) alist)))))

(defun pjones:editorconfig-apply-trim-whitespace (props)
  "Conditionally change the trim_trailing_whitespace setting.

Enable whitespace trimming unless the editor configuration file
explicitly disables it.  PROPS is the hash from the editor configuration
file."
  (when (null (gethash 'trim_trailing_whitespace props))
    (puthash 'trim_trailing_whitespace "true" props)
    (editorconfig--get-trailing-ws props)))

(add-hook 'editorconfig-get-local-variables-functions
          #'pjones:editorconfig-apply-trim-whitespace 50)

;;; editorconfig-conf.el ends here
