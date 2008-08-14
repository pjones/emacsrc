;; muse-barecms.el --- publish HTML output with BareCMS
;;
;; Copyright (C) 2008 pmade inc. (Peter Jones pjones@pmade.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:
;;
;; Publish files to BareCMS via Emacs Muse.
(require 'muse-publish)
(require 'muse-project)
(require 'muse-html)

(eval-when-compile
  (require 'cl))

(defgroup muse-barecms nil
  "Options controlling the behavior of Muse BareCMS publishing."
  :group 'muse-publish)

(defcustom muse-barecms-extension ".html"
  "Default file extension for publishing BareCMS files."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-link-suffix ""
  "Default suffix added to links."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-author-date-separator " / "
  "The text placed between the author and date in the header."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-header
  "<!-- Published with Emacs Muse and BareCMS -->
<div class=\"heading\">
  <h1 class=\"title\"><lisp>(muse-publishing-directive \"title\")</lisp></h1>
  <lisp>
    (let ((author (muse-barecms-publishing-directive \"author\"))
          (date   (muse-barecms-publishing-directive \"date\"))
          (sep    muse-barecms-author-date-separator)
          (attribution \"\"))
      (when (or author date) (setq attribution (concat attribution \"<div class=\\\"attribution\\\">\")))
      (when author (setq attribution (concat attribution \"<span class=\\\"author\\\">by \" author \"</span>\" (when date sep))))
      (when date (setq attribution (concat attribution \"<span class=\\\"date\\\">\" (muse-barecms-formatted-date) \"</span>\")))
      (when (or author date) (setq attribution (concat attribution \"<br class=\\\"clear\\\"/></div>\")))
      attribution)
   </lisp>
</div>\n"
  "Header used for publishing BareCMS files.  This may be text or a filename."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-footer ""
  "Footer used for publishing BareCMS files.  This may be text or a filename."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-stage "Live"
  "The default stage to publish to."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-base-path "/"
  "A path that is prefixed to the URL path for a published file.
Usually this will be a forward slash, but if you are publishing
a file to a sub-directory on the BareCMS site, you'll need to
adjust this."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-url "http://localhost:3000"
  "The protocol, hostname, and port number parts of the URL to use."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-admin-path "/admin/pages/upload"
  "The URL path for the BareCMS page upload handler."
  :type 'string :group 'muse-barecms)

(defcustom muse-barecms-curl-extra "--netrc-optional -m 120 -s"
  "Extra command line arguments to pass to cURL."
  :type 'string :group 'muse-barecms)

;; END of customizations

(defvar muse-barecms-curl-command-line nil
  "The cURL command line that is about to be used.")

(defun muse-barecms-publishing-directive (name &optional missing-directive)
  "Like muse-publishing-directive, but turns \"nil\" into missing-directive."
  (let ((value (muse-publishing-directive name)))
    (cond
     ((and value (string= value "nil")) missing-directive)
     ((not value) missing-directive)
     (t value))))

(defun muse-barecms-calculate-remote-path (source-file)
  "Figure out the full path to the remote file in BareCMS."
  (concat muse-barecms-base-path
          (file-relative-name
           (file-name-sans-extension (expand-file-name source-file))
           (expand-file-name (caadr (muse-project-of-file source-file))))))

(defun muse-barecms-formatted-date ()
  "Returns the date directive, formatted using muse-barecms-date-format."
  (let ((date (muse-publishing-directive "date")))
    (format-time-string
     muse-publish-date-format
     (apply 'encode-time 0 0 0
            (mapcar 'string-to-number (reverse (split-string date "-")))))))

(defun muse-barecms-full-url ()
  "Calculate the full URL needed to post a page."
  (concat muse-barecms-url muse-barecms-admin-path))

(defun muse-barecms-upload (source-file published-file)
  "Upload the given file."
  (muse-barecms-upload-curl source-file published-file))

(defun muse-barecms-curl-cmd (source-file published-file)
  "Calculate the cURL command line."
  (concat "curl " muse-barecms-curl-extra " "
          "-F 'page[published_at]='" (shell-quote-argument (muse-barecms-publishing-directive "date" ""))    " "
          "-F 'page[author_name]='"  (shell-quote-argument (muse-barecms-publishing-directive "author" ""))  " "
          "-F 'page[title]='"        (shell-quote-argument (muse-barecms-publishing-directive "title" ""))   " "
          "-F 'page[tags]='"         (shell-quote-argument (muse-barecms-publishing-directive "tags" ""))    " "
          "-F 'page[path]='"         (shell-quote-argument (muse-barecms-calculate-remote-path source-file)) " "
          "-F 'page[stage]='"        (shell-quote-argument muse-barecms-stage) " "
          "-F 'page[body]=<'"        (shell-quote-argument published-file)     " "
          (muse-barecms-full-url)))

(defun muse-barecms-upload-curl (source-file published-file)
  "Upload the given file using the cURL command line tool."
  (let ((result (shell-command muse-barecms-curl-command-line)))
    (if (= 0 result) (message "Published via BareCMS")
      (error "Error publishing to BareCMS"))))

(defun muse-barecms-deploy (source-file published-file second-stage-file)
  "Called from Emacs Muse after a file has been published."
  (muse-barecms-upload source-file published-file))

(defun muse-barecms-prepare ()
  "Prepare to upload a file to BareCMS"
  (when (not muse-current-project) (error "BareCMS files must be in a Muse project"))
  (setq muse-barecms-curl-command-line
        (muse-barecms-curl-cmd
         muse-publishing-current-file
         muse-publishing-current-output-path)))

;; Tell Emacs Muse about our style
(muse-derive-style "barecms" "xhtml"
                   :suffix      'muse-barecms-extension
                   :link-suffix 'muse-barecms-link-suffix
                   :header      'muse-barecms-header
                   :footer      'muse-barecms-footer
                   :after       'muse-barecms-prepare
                   :final       'muse-barecms-deploy)

(provide 'muse-barecms)
