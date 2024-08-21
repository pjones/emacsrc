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
 '(org-roam-directory (concat pjones:org-notes-directory "wiki"))
 '(org-roam-dailies-directory "journal")
 '(org-roam-db-node-include-function
   #'org-before-first-heading-p) ; Only files are org-roam nodes.

 '(consult-org-roam-buffer-enabled nil) ; Treat like other buffers.

 '(org-roam-capture-templates
   '(("b" "Knowledge Base" plain "%?"
      :target (file+head "garden/${pjones:org-roam-node-to-file}" "#+title: ${title}\n")
      :jump-to-captured t
      :unnarrowed t)))

 '(org-roam-dailies-capture-templates
   '(("d" "Daily Journal" entry
     "* %?\n  %(pjones:org-time-stamp t)\n"
     :target (file+head "%<%Y>/%<%m>/%<%Y-%m-%d>.org" "#+title: %<%A, %B %d, %Y>\n#+date: %u\n")
     :unnarrowed t
     :jump-to-captured t
     :empty-lines-before 1)
     ("m" "Monthly Journal" entry
      ""
      :target (file+head "%<%Y>/%<%m>/index.org" "#+title: %<%B, %Y>\n")
      :unnarrowed t
      :empty-lines-before 1)
     ("h" "Mental Health" entry
      (file "~/notes/templates/org/mental-health.org")
      :target (file+head "%<%Y>/%<%m>/%<%Y-%m-%d>.org" "#+title: %<%A, %B %d, %Y>\n#+date: %u\n")
      :empty-lines-before 1))))

(defun pjones:org-roam-buffer-name ()
  "Set the buffer name using the org title."
  (when-let* ((title (cadr (assoc "TITLE" (org-collect-keywords '("title")))))
              (file (buffer-file-name))
              (base (file-name-nondirectory file)))
    (rename-buffer title)))

(defun pjones:org-roam-node-to-file (node)
  "Convert the NODE's ID to a file name."
  (let ((name (org-roam-node-id node)))
    (concat (substring name 0 1) "/"
            (substring name 1 2) "/"
            name ".org")))

(defun pjones:org-publish-sitemap (title list)
  "Alternate sitemap generation function.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'."
  (concat "#+title: " title "\n"
          "#+html_link_home:\n#+html_link_up:\n\n"
          (org-list-to-org list)))

(defun pjones:org-roam-before-publish (&rest _)
  "Hook run before publishing begins."
  (org-roam-db-sync)
  (org-id-update-id-locations)
  (org-roam-update-org-id-locations))

(defun pjones:org-roam-after-publish (&rest _)
  "Hook run after publishing ends."
  (delete-file (concat org-roam-directory "/sitemap.org")))

;; Adapted from: https://org-roam.discourse.group/t/export-backlinks-on-org-export/1756/24
(defun pjones:org-roam-insert-backlinks (backend)
  "Insert `org-roam' back links during HTML export.
BACKEND is a symbol indicating the current publishing back-end."
  (when (eq backend 'html)
    (when-let* ((source-node (org-roam-node-at-point))
                (source-file (org-roam-node-file source-node))
                (nodes-in-file (cl-remove-if-not
                                (lambda (item)
                                  (string-equal (org-roam-node-file item)
                                                source-file))
                                (org-roam-node-list)))
                ;; Nodes don't store the last position, so get the next
                ;; headline position and subtract one character (or, if
                ;; no next headline, get point-max)
                (nodes-and-end
                 (mapcar
                  (lambda (node)
                    (let* ((start (org-roam-node-point node))
                           (end (save-excursion
                                  (goto-char start)
                                  (if (org-before-first-heading-p) ;; file node
                                      (point-max)
                                    (call-interactively #'org-forward-heading-same-level)
                                    (if (> (point) start)
                                        (- (point) 1) ;; successfully found next
                                      (point-max)))))) ;; there was no next
                      (cons node end)))
                  nodes-in-file)))
      (dolist (node-and-end (sort nodes-and-end (lambda (a b) (> (cdr a) (cdr b)))))
        (when-let* ((node (car node-and-end))
                    (end-position (cdr node-and-end))
                    (backlinks (org-roam-backlinks-get node))
                    (heading (format "\n\n%s Related Notes\n"
                                     (make-string (+ (org-roam-node-level node) 1) ?*)))
                    (properties-drawer ":PROPERTIES:\n:HTML_CONTAINER_CLASS: references\n:END:\n"))
          (goto-char end-position)
          (insert heading)
          (insert properties-drawer)
          (dolist (backlink backlinks)
            (let* ((source-node (org-roam-backlink-source-node backlink))
                   (source-file (org-roam-node-file source-node))
                   (properties (org-roam-backlink-properties backlink))
                   (outline (when-let ((outline (plist-get properties :outline)))
                              (when (> (length outline) 1)
                                (mapconcat #'org-link-display-format outline " > "))))
                   (reference (format "%s [[id:%s][%s]]\n%s\n\n"
                                      (make-string (+ (org-roam-node-level node) 2) ?*)
                                      (org-roam-node-id source-node)
                                      (org-roam-node-title source-node)
                                      (if outline
                                          (format
                                           "%s (/%s/)"
                                           (make-string (+ (org-roam-node-level node) 3) ?*)
                                           outline)
                                        "")))
                   (label-list (with-temp-buffer
                                 (org-element-map (org-element-parse-buffer) 'footnote-reference
                                   (lambda (reference)
                                     (org-element-property :label reference)))))
                   (footnote-string-list
                    (with-temp-buffer
                      (insert-file-contents source-file)
                      (mapcar (lambda (label) (buffer-substring-no-properties
                                          (nth 1 (org-footnote-get-definition label))
                                          (nth 2 (org-footnote-get-definition label))))
                              label-list))))
              (mapc (lambda (footnote-string) (insert footnote-string)) footnote-string-list)
              (insert reference))))))))

;; Ensure the database is up-to-date:
(org-roam-db-autosync-mode)

;; Activate the consult helper mode:
(consult-org-roam-mode)

;; Hooks:
(add-hook 'org-export-before-processing-functions #'pjones:org-roam-insert-backlinks)
(add-hook 'org-roam-find-file-hook #'pjones:org-roam-buffer-name)

;;; org-roam-conf.el ends here
