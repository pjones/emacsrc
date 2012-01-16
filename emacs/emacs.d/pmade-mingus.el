;;; Mingus (front-end to MPD)
(setq mingus-use-ido-mode-p t
      mingus-playlist-separator " » "
      mingus-stream-alist
      '(("AbaClub"              . "http://listen.radionomy.com/abaclub")
        ("AccentMix"            . "http://live.it.acc.vmix.fr:8000/")
        ("AllectroRadio"        . "http://listen.radionomy.com/allectro-radio")
        ("BarockMusic"          .  "http://listen.radionomy.com/barock-music")
        ("Beat Blender"         . "http://steady.somafm.com:8388")
        ("Blissful"             . "http://67.212.189.10:8030/")
        ("Bluegrass"            . "http://wamu-2.streamguys.com/")
        ("Doomed"               . "http://voxsc1.somafm.com:8300")
        ("Drone Zone"           . "http://streamer-ntc-aa03.somafm.com:80/stream/1032")
        ("Groove Salad"         . "http://streamer-dtc-aa02.somafm.com:80/stream/1018")
        ("Ill Street Lounge"    . "http://steady.somafm.com:8500")
        ("KGNU"                 . "http://stream.kgnu.net:8000/KGNU_live_high.mp3")
        ("Lush"                 . "http://streamer-ntc-aa06.somafm.com:80/stream/1073")
        ("Pig Radio"            . "http://s6.mediastreaming.it:8080")
        ("PopTron"              . "http://voxsc1.somafm.com:2200")
        ("Secret Agent"         . "http://streamer-dtc-aa03.somafm.com:80/stream/1021")
        ("Space Station"        . "http://mp2.somafm.com:2666")
        ("Suburbs of Goa"       . "http://voxsc1.somafm.com:8850")
        ("Tag's Trip"           . "http://205.188.215.230:8012")
        ("Underground Eighties" . "http://voxsc1.somafm.com:8880")))

(defun pmade:mingus-hook ()
  "Set up all of the mingus buffers in a similar way."
  (hl-line-mode t)
  (flyspell-mode -1))

(add-hook 'mingus-playlist-hooks 'pmade:mingus-hook)
(add-hook 'mingus-browse-hook    'pmade:mingus-hook)
