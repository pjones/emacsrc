;;; time-conf.el -- Settings for `time' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Settings for `world-clock'.
;;
;;; Code:

(require 'time)

(custom-set-variables
 '(zoneinfo-style-world-list
   '(("America/Los_Angeles" "Los Angeles")
     ("America/Phoenix" "Phoenix")
     ("America/Denver" "Denver")
     ("America/New_York" "New York")
     ("UTC" "UTC")
     ("Europe/Paris" "Tübingen")
     ("Asia/Tokyo" "Tokyo"))))

;;; time-conf.el ends here
