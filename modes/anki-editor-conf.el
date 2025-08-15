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
  (rx word-boundary
      (group (or "der" "das" "die"))
      (+ (char blank))
      (group (char upper)
             (+ (char word ?  ?-)))
      (seq ","
           (+ (char blank))
           (or "-" "⸚")
           (* (char word ?/))))
  "Regular expression to match German nouns.")

(defvar pjones:anki-de-noun-nur-plural-re
  (rx word-boundary
      "die"
      (+ (char blank))
      (group (char upper)
             (+ (char word ?  ?-)))
      " (nur Pl.)")
  "Regular expression to match nouns that are only plural.")

(defvar pjones:anki-media-subdir "../media/"
  "Directory where media files are stored.")

(defvar pjones:anki-download-url-history nil
  "History of URLs read from the user.")

(defvar pjones:anki-de-noun-gender-table nil
  "A lookup table for noun to gender translation.")

(defun pjones:anki-de-noun-gender-filter (text backend info)
  "Tag nouns in TEXT with their gender.
Only works when BACKEND is the `anki-editor' backend or INFO indicates
the same."
  (let* ((backend (or backend (plist-get info :back-end)))
         (case-fold-search nil)
         (noun-re (rx word-boundary
                      (group (char upper)
                             (+ (char word ? )))
                      word-boundary))
         (span (lambda (gender body)
                 (concat "<span class=\"noun "
                         gender "\">"
                         body "</span>")))
         (replace (lambda (gender)
                    (replace-match (funcall span gender "\\&")
                                   t nil text))))
    (when (eq backend anki-editor--ox-anki-html-backend)
      (cond
       ((string-match pjones:anki-de-noun-gender-re text)
        ;; Defines a gendered noun.
        (funcall replace (match-string 1 text)))
       ((string-match pjones:anki-de-noun-nur-plural-re text)
        ;; Defines a plural-only noun.
        (funcall replace "plu"))
       (t
        ;; Might reference an existing noun.
        (let ((pos 0)
              (final text))
          (while-let ((npos (string-match noun-re final pos))
                      (noun (match-string-no-properties 1 final)))
            (setq pos (+ npos (length noun)))
            (when-let* ((gender (gethash noun pjones:anki-de-noun-gender-table))
                        (tagged (funcall span gender noun)))
              (setq pos (+ npos (length tagged))
                    final (replace-match tagged t t final 1))))
          final))))))

(defun pjones:anki-de-noun-gender-scan ()
  "Return a hash table of noun to gender mappings."
  (let ((nouns (make-hash-table :test 'equal))
        (regex (concat pjones:anki-de-noun-gender-re "\\|"
                       pjones:anki-de-noun-nur-plural-re)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward regex nil t)
          (cond
           ((and (match-string 1) (match-string 2)) ; der, das, or die
            (puthash (match-string-no-properties 2)
                     (match-string-no-properties 1)
                     nouns))
           ((match-string 3) ; plural
            (puthash (match-string-no-properties 3) "plu" nouns))))))
    nouns))

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

(defun pjones:anki-editor-push-notes ()
  "Export Anki notes with some magic."
  (interactive)
  (setq pjones:anki-de-noun-gender-table
        (pjones:anki-de-noun-gender-scan))
  (call-interactively #'anki-editor-push-notes))

(defun pjones:anki-editor-mode-hook ()
  "Hook for `anki-editor-mode'."
  (keymap-set anki-editor-mode-map "C-c C-e a" #'pjones:anki-editor-push-notes)
  (keymap-set anki-editor-mode-map "C-c C-a m" #'pjones:anki-media)

  ;; This can't be buffer local because anki-editor uses
  ;; `org-export-string-as' which creates a new buffer.
  (add-hook 'org-export-filter-plain-text-functions
            #'pjones:anki-de-noun-gender-filter))

(custom-set-variables
 '(anki-editor-include-default-style nil)
 '(anki-editor-latex-style 'mathjax)
 '(anki-editor-field-alias
   '(("Deutsch" . (("Deutsch"          . "Front")
                   ("Englisch"         . "Back")
                   ("Deutsche Notizen" . "Notes"))))))

(add-hook 'anki-editor-mode-hook #'pjones:anki-editor-mode-hook)

;;; anki-editor-conf.el ends here
