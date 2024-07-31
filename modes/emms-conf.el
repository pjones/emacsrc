;;; emms-conf.el -- Settings for `emms' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'emms)
(require 'emms-mpris)
(require 'emms-setup)

(emms-all)
(emms-default-players)

;; Make the compiler happy:
(defvar emms-streams-built-in-list)

(defun pjones:emms-track-description-function (track)
  "Return a string description of TRACK."
  (let ((type (emms-track-type track)))
    (cond
     ((eq type 'streamlist)
      (format "%-40s (%s)"
              (car (emms-track-get track 'metadata))
              (emms-track-get track 'name)))
     ((eq type 'file)
      (let* ((name (emms-track-name track))
             (artist (emms-track-get track 'info-artist))
             (year (emms-track-get track 'info-year))
             (num (emms-track-get track 'info-tracknumber))
             (title (when-let ((name (emms-track-get track 'info-title)))
                      (if num (format "[%5s] %s" num name) name)))
             (album (when-let ((name (emms-track-get track 'info-album)))
                      (if year (format "%s (%s)" name year) name))))
        (if (and artist album title)
            (format "%-40s %-20s %-20s" title artist album)
          name)))
     (t (emms-track-simple-description track)))))

(defun pjones:emms-play-stream ()
  "Prompt for a stream and then tell EMMS to play it."
  (interactive)
  (when-let* ((metadata (mapcar (lambda (track)
                                  (cdr track))
                                emms-streams-built-in-list))
              (streams (mapcar (lambda (track)
                                 (cons (car (alist-get 'metadata track)) track))
                               metadata))
              (choice (completing-read "Stream: " streams))
              (track (assoc choice streams 'string-equal))
              (type (alist-get 'type (cdr track)))
              (url (alist-get 'name (cdr track))))
    (cond
     ((equal 'streamlist type)
      (emms-play-streamlist url))
     ((equal 'url type)
      (emms-play-url url))
     (t (message "I don't know how to play stream type %s" type)))))

(defun pjones:emms-playlist-insert-track (track)
  "Insert TRACK after ensuring it has a good path.

My playlists use relative file paths and EMMS doesn't like that.  If
TRACK is either relative, or if EMMS has inferred that the file is in
my playlists directory then this function will fix the path so it is
absolute to the music directory."
  (let ((path (emms-track-name track))
        (plst-dir (concat
                   (file-name-directory
                    (directory-file-name emms-source-file-default-directory))
                   "playlists/")))
    (cond
     ((and (eq 'file (emms-track-type track))
           (not (file-name-absolute-p path)))
      (emms-track-set
       track 'name
       (concat emms-source-file-default-directory path)))
     ((and (eq 'file (emms-track-type track))
           (string-prefix-p plst-dir path))
      (emms-track-set
       track 'name
       (concat emms-source-file-default-directory
               (substring path (length plst-dir)))))))
  (emms-playlist-simple-insert-track track))

(defun pjones:mkstream (name url &optional type)
  "Return a streamlist structure expected by EMMS.
The name of the streams is passed in NAME and the playlist URL in
URL.  If the track type isn't streamlist you must provide TYPE."
  (interactive)
  (setq type (or type 'streamlist))
  `(*track* (type . ,type)
            (name . ,url)
            (metadata ,name ,url 1 ,type)))

(custom-set-variables
 '(emms-source-file-default-directory (expand-file-name "~/documents/music/"))
 '(emms-mode-line-mode-line-function nil)
 '(emms-mode-line-format "")
 '(emms-player-mpv-update-metadata t)
 '(emms-streams-file (expand-file-name "~/documents/playlists/streams.emms"))
 '(emms-streams-built-in-disclaimer "")
 '(emms-playlist-insert-track-function #'pjones:emms-playlist-insert-track)
 '(emms-track-description-function #'pjones:emms-track-description-function)
 '(emms-streams-built-in-list
   `(,(pjones:mkstream
       "Beat Blender [SomaFM]"
       "http://www.somafm.com/beatblender.pls")

     ,(pjones:mkstream
       "Secret Agent [SomaFM]"
       "http://www.somafm.com/secretagent.pls")

     ,(pjones:mkstream
       "Groove Salad [SomaFM]"
       "http://www.somafm.com/groovesalad.pls")

     ,(pjones:mkstream
       "DEF CON Radio [SomaFM]"
       "http://somafm.com/defcon.pls")

     ,(pjones:mkstream
       "Doomed [SomaFM]"
       "http://somafm.com/doomed.pls")

     ,(pjones:mkstream
       "Classic Groove Salad [SomaFM]"
       "http://somafm.com/gsclassic.pls")

     ,(pjones:mkstream
       "Illinois Street Lounge [SomaFM]"
       "http://somafm.com/illstreet.pls")

     ,(pjones:mkstream
       "Lush [SomaFM]"
       "http://somafm.com/lush.pls")

     ,(pjones:mkstream
       "PopTron [SomaFM]"
       "http://somafm.com/poptron.pls")

     ,(pjones:mkstream
       "Suburbs of Goa [SomaFM]"
       "http://somafm.com/suburbsofgoa.pls")

     ;; https://somafm.com/u80s/
     ,(pjones:mkstream
       "Underground 80s [SomaFM]"
       "http://somafm.com/u80s.pls")

     ,(pjones:mkstream
       "Drone Zone [SomaFM]"
       "http://www.somafm.com/dronezone.pls")

     ;; https://www.laradioplus.com/radio/history
     ,(pjones:mkstream
       "La Radio Plus"
       "http://laradioplus.ice.infomaniak.ch/laradioplus-high.mp3"
       'url)

     ;; https://www.radiofrance.fr/francemusique
     ,(pjones:mkstream
       "Classique Plus [France Musique]"
       "http://direct.francemusique.fr/live/francemusiqueclassiqueplus-hifi.mp3"
       'url)

     ;; https://wfmu.org/drummer
     ,(pjones:mkstream
       "Give the Drummer Radio [WFMU]"
       "http://www.wfmu.org/wfmu.pls"))))

(defun pjones:emms-mpris-change-status (_track)
  "Update MPRIS on metadata change."
  (emms-mpris-change-status))

(defun pjones:emms-playlist-mode-hook ()
  "Hook run when a playlist is created."
  ;; Set some buffer local functions:
  (setq-local emms-playlist-insert-track-function #'pjones:emms-playlist-insert-track)
  (let ((map emms-playlist-mode-map))
    (define-key map (kbd "d") #'emms-add-directory)
    (define-key map (kbd "D") #'emms-playlist-mode-goto-dired-at-point)
    (define-key map (kbd "i") #'emms-insert-playlist)
    (define-key map (kbd "SPC") #'emms-pause)))

;; Hooks:
(add-hook 'emms-track-updated-functions
          #'pjones:emms-mpris-change-status)

(add-hook 'emms-playlist-mode-hook #'pjones:emms-playlist-mode-hook)

;;; emms-conf.el ends here
