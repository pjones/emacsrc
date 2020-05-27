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
  "Ensure that the `org-roam' display is visible."
  (interactive)
  (when (org-roam--org-roam-file-p)
    (require 'org-roam-protocol)
    (require 'company-org-roam)
    (push 'company-org-roam company-backends)
    (pcase (org-roam-buffer--visibility)
      ('none
       (org-roam-mode) ; Ensure this is enabled.
       (org-roam-buffer--get-create)))))

;; Patch broken function:
(defun org-roam-buffer--get-create ()
  "Set up the `org-roam' buffer at `org-roam-buffer-position'."
  (let* ((position
         (if (member org-roam-buffer-position '(right left top bottom))
             org-roam-buffer-position
           (let ((text-quoting-style 'grave))
             (lwarn '(org-roam) :error
                    "Invalid org-roam-buffer-position: %s. Defaulting to \\='right"
                    org-roam-buffer-position))
           'right))
         (params `((no-other-window . t)
                   (no-delete-other-windows
                    . ,org-roam-buffer-no-delete-other-windows)))
         (buf (get-buffer-create org-roam-buffer))
         (win (display-buffer-in-side-window
               buf `((side . ,position) (window-parameters . ,params)))))
    (save-selected-window
      (select-window win)
      (set-window-dedicated-p win t)
      (pcase position
        ((or 'right 'left)
         (org-roam-buffer--set-width
          (round (* (frame-width)  org-roam-buffer-width))))
        ((or 'top  'bottom)
         (org-roam-buffer--set-height
          (round (* (frame-height) org-roam-buffer-height))))))))

;; Patch to be case-insensitive:
(defun org-roam--extract-global-props (props)
  "Extract PROPS from the current org buffer.
The search terminates when the first property is encountered."
  (let ((buf (org-element-parse-buffer))
        res)
    (dolist (prop props)
      (let ((p (org-element-map buf 'keyword
                 (lambda (kw)
                   (when (string-collate-equalp
                          (org-element-property :key kw) prop nil t)
                     (org-element-property :value kw)))
                 :first-match t)))
        (push (cons prop p) res)))
    res))

;;; org-roam-conf.el ends here
