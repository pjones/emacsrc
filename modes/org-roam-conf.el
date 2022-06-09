;;; org-roam-conf.el -- Settings for `org-roam' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Interesting hacks:
;;
;;   - The default capture template places org files into a nested
;;     folder structure based on a generated org ID.  The name of the
;;     file is the entire ID plus the ".org" extension.
;;
;;     I do this to limit the number of files in any given directory.
;;     Some operating systems (Android) don't deal well with a
;;     directory that contains hundreds of files.
;;
;;   - Since the file names are just IDs, buffers are renamed so that
;;     they contain the org title and the file name.
;;
;;; Code:

(require 'consult-org-roam)
(require 'org-roam)
(require 'org-roam-export)

(custom-set-variables
 '(org-roam-directory "~/notes/wiki")
 '(org-roam-dailies-directory "journal")

 '(org-roam-capture-templates
   '(("b" "Knowledge Base" plain "%?"
      :target (file+head "garden/${pjones:org-roam-node-to-file}" "#+title: ${title}\n")
      :unnarrowed t)))

 '(org-roam-dailies-capture-templates
   '(("d" "Daily Journal" entry
     "* %U %?\n"
     :target (file+head "%<%Y>/%<%Y-%m-%d>.org" "#+title: %<%A, %B %d, %Y>\n")
     :unnarrowed t
     :empty-lines-before 1))))

(defun pjones:org-roam-buffer-name ()
  "Set the buffer name using the org title."
  (when-let* ((title (cadr (assoc "TITLE" (org-collect-keywords '("title")))))
              (file (buffer-file-name))
              (base (file-name-nondirectory file)))
    (rename-buffer (concat title " (" base ")"))))

(defun pjones:org-roam-node-to-file (node)
  "Convert the NODE's ID to a file name."
  (let ((name (org-roam-node-id node)))
    (concat (substring name 0 1) "/"
            (substring name 1 2) "/"
            name ".org")))

(defun pjones:org-roam-publish (&optional force)
  "Publish Peter's `org-roam' wiki.
If FORCE is non-nil then rebuild the entire site."
  (interactive "P")
  (let ((org-export-with-broken-links 'mark)
        (org-html-validation-link nil))
    (org-roam-db-sync)
    (org-id-update-id-locations)
    (org-roam-update-org-id-locations)
    (org-publish
     `("wiki"
       :base-directory ,org-roam-directory
       :base-extension "org"
       :recursive t
       :auto-sitemap t
       :sitemap-title "Peter's Knowledge Base (All Pages)"
       :sitemap-filename "sitemap.org"
       :sitemap-sort-folders ignore
       :sitemap-style list
       :publishing-function org-html-publish-to-html
       :publishing-directory "~/public/wiki"
       :section-numbers t
       :with-toc nil)
     force nil))
  (delete-file (concat org-roam-directory "/sitemap.org")))

;; Ensure the database is up-to-date:
(org-roam-db-autosync-mode)

;; Activate the consult helper mode:
(consult-org-roam-mode)

;; Hooks:
(add-hook 'org-roam-find-file-hook #'pjones:org-roam-buffer-name)

;;; org-roam-conf.el ends here
