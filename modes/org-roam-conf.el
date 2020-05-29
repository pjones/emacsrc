;;; org-roam-conf.el -- Settings for org-roam
;;
;;; Commentary:
;;
;;; Code:
(require 'org-roam)
(require 'company)

(custom-set-variables
 `(org-roam-directory ,(expand-file-name "~/notes/zettelkasten/"))
 '(org-roam-graph-viewer 'find-file)
 '(org-roam-buffer-no-delete-other-windows t)
 '(org-roam-capture-templates
   '(("d" "default" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n\n"
      :unnarrowed t)
     ("r" "ref" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n#+roam_key: ${ref}\n\n"
      :unnarrowed t))))

(defun pjones:org-roam-activate ()
  "Ensure that the `org-roam' is ready and displayed."
  (interactive)
  (when (org-roam--org-roam-file-p)
    (require 'org-roam-protocol)
    (require 'company-org-roam)
    (org-roam-mode) ; Ensure this is enabled.
    (push 'company-org-roam company-backends)
    (pcase (org-roam-buffer--visibility)
      ((or 'exists 'none) (org-roam-buffer--get-create)))))

;;; org-roam-conf.el ends here
