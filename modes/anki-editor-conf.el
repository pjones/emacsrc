;;; anki-editor-conf.el -- Settings for `anki-editor' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'anki-editor)

(declare-function google-translate-format-listen-url "google-translate-core")
(declare-function pjones:shell-command "../lisp/shell")
(declare-function pjones:url-file-name "../lisp/functions")
(defvar org-capture-templates)

(defvar pjones:anki-de-noun-gender-re
  (rx (seq word-boundary
           (group (or "der" "das" "die"))
           (+ (char blank))
           (char upper)
           (+ (char word))
           (seq ","
                (+ (char blank))
                (or "-" "⸚")
                (* (char word ?/)))))
  "Regular expression to match German nouns.")

(defvar pjones:anki-media-subdir "media/"
  "Directory where media files are stored.")

(defvar pjones:anki-download-url-history nil
  "History of URLs read from the user.")

(defun pjones:anki-de-noun-gender-filter (text backend info)
  "Tag nouns in TEXT with their gender.
Only works when BACKEND is the `anki-editor' backend or INFO indicates
the same."
  (let ((backend (or backend (plist-get info :back-end)))
        (case-fold-search nil))
    (if (and (eq backend anki-editor--ox-anki-html-backend)
             (string-match pjones:anki-de-noun-gender-re text))
        (let* ((gender (match-string 1 text))
               (replacement (concat "<span class=\"noun " gender "\">\\&</span>")))
          (replace-match replacement t nil text)))))

(defun pjones:anki-download-media-insert (marker path)
  "Sentinel function for the download buffer.
MARKER is where the new link should be placed.  PATH is the path to the
the file that will be linked to."
  (if (member (downcase (file-name-extension path)) '("ogg" "wav" "mpeg"))
      (let ((newpath (concat (file-name-sans-extension path) ".mp3")))
        (pjones:shell-command :bufname "ffmpeg"
                              :command (list "ffmpeg" "-i" path newpath)
                              :close t
                              :success (lambda ()
                                         (delete-file path)
                                         (pjones:anki-download-media-insert
                                          marker
                                          newpath))))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (insert (org-link-make-string (concat "file:"
                                            (file-relative-name path)))))))

(defun pjones:anki-media-download (url &optional filename)
  "Download URL into the media directory and insert a file link.
If FILENAME is non-nil use it as the file name instead of guessing."
  (interactive (list (read-string "URL: " nil 'pjones:anki-download-url-history)))
  (let* ((file (expand-file-name
                (read-file-name
                 "Download to file: "
                 pjones:anki-media-subdir
                 nil nil
                 (replace-regexp-in-string
                  (rx (+ (not (any word ?.)))) "-"
                  (or filename (pjones:url-file-name url))))))
         (marker (point-marker))
         (callback (apply-partially #'pjones:anki-download-media-insert marker file)))
    (if (or (not (file-exists-p file))
            (yes-or-no-p "File exists, overwrite? "))
        (pjones:shell-command :bufname "curl"
                              :command (list "curl" "-o" file url)
                              :close t
                              :success callback))))

(defun pjones:anki-media-tts (begin end)
  "Generate an audio file and link to it.
TEXT to translate is taken from BEGIN to END."
  (interactive "r")
  (require 'google-translate-core)
  (let ((text (buffer-substring-no-properties begin end)))
    (end-of-line)
    (newline-and-indent 2)
    (pjones:anki-media-download
     (google-translate-format-listen-url text "de")
     (concat (replace-regexp-in-string
              (rx (+ (not (any word blank)))) ""
              (downcase text))
             ".mpeg"))))

(defun pjones:anki-media ()
  "Download media or use TTS."
  (interactive)
  (call-interactively
   (if (use-region-p) #'pjones:anki-media-tts
     #'pjones:anki-media-download)))

(defun pjones:anki-template-insert ()
  "Insert a template in the current tree.
The template file name will be taken from the TEMPLATE property in the
current tree."
  (interactive)
  (let* ((key "A")
         (file (or (org-entry-get nil "TEMPLATE" t) "template.org"))
         (id (org-entry-get nil "ID" t))
         (template `(,key "Anki Template" entry
                          ,(if id (list 'id id) '(here))
                          (file ,(if (file-name-absolute-p file) file
                                   (concat default-directory file)))
                          :empty-lines 1
                          :immediate-finish t
                          :jump-to-captured t))
         (org-capture-templates (list template)))
    (org-capture nil key)))

(defun pjones:anki-editor-mode-hook ()
  "Hook for `anki-editor-mode'."
  (keymap-local-set "C-c C-e a" #'anki-editor-push-notes)
  (keymap-local-set "C-c i" #'pjones:anki-template-insert)
  (keymap-local-set "C-c m" #'pjones:anki-media))

(custom-set-variables
 '(anki-editor-include-default-style nil)
 '(anki-editor-field-alias
   '(("Deutsch" . (("Deutsch"          . "Front")
                   ("Englisch"         . "Back")
                   ("Deutsche Notizen" . "Notes"))))))

(add-hook 'anki-editor-mode-hook #'pjones:anki-editor-mode-hook)

(add-hook 'org-export-filter-plain-text-functions
          #'pjones:anki-de-noun-gender-filter)

;;; anki-editor-conf.el ends here
