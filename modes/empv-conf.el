;;; empv-conf.el -- Settings for `empv' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'empv)

(custom-set-variables
 '(empv-audio-dir (concat (getenv "XDG_MUSIC_DIR") "/audio"))
 '(empv-playlist-dir (concat (getenv "XDG_MUSIC_DIR") "/playlists"))
 '(empv-display-events nil)

 '(empv-radio-channels
   '(("Beat Blender [SomaFM]" . "http://www.somafm.com/beatblender.pls")
     ("Bossa Beyond [SomaFM]" . "https://somafm.com/bossa.pls")
     ("Classic Groove Salad [SomaFM]" . "http://somafm.com/gsclassic.pls")
     ("Classique Plus [France Musique]" . "http://direct.francemusique.fr/live/francemusiqueclassiqueplus-hifi.mp3")
     ("DEF CON Radio [SomaFM]" . "http://somafm.com/defcon.pls")
     ("Doomed [SomaFM]" . "http://somafm.com/doomed.pls")
     ("Drone Zone [SomaFM]" . "http://www.somafm.com/dronezone.pls")
     ("Give the Drummer Radio [WFMU]" . "http://www.wfmu.org/wfmu.pls")
     ("Groove Salad [SomaFM]" . "http://www.somafm.com/groovesalad.pls")
     ("Illinois Street Lounge [SomaFM]" . "http://somafm.com/illstreet.pls")
     ("KEXP 90.3 FM Seattle" . "https://kexp.streamguys1.com/kexp64.aac")
     ("La Radio Plus" . "http://laradioplus.ice.infomaniak.ch/laradioplus-high.mp3")
     ("Lush [SomaFM]" . "http://somafm.com/lush.pls")
     ("PopTron [SomaFM]" . "http://somafm.com/poptron.pls")
     ("Secret Agent [SomaFM]" . "http://www.somafm.com/secretagent.pls")
     ("Suburbs of Goa [SomaFM]" . "http://somafm.com/suburbsofgoa.pls")
     ("Tiki Time [SomaFM]" . "https://somafm.com/tikitime.pls")
     ("Underground 80s [SomaFM]" . "http://somafm.com/u80s.pls"))))

;;; empv-conf.el ends here
