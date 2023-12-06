;;; ement-conf.el -- Settings for `ement' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'ement)

(custom-set-variables
 '(ement-room-prism 'both)
 '(ement-room-send-message-filter #'ement-room-send-org-filter)

 ;; Notifications:
 '(ement-notify-notification-predicates
   '(ement-notify--event-mentions-session-user-p))
 '(ement-notify-log-predicates
   '(ement-notify--event-mentions-session-user-p))
 '(ement-notify-mention-predicates
   '(ement-notify--event-mentions-session-user-p))
 '(ement-notify-mark-frame-urgent-predicates
   '(ement-notify--event-mentions-session-user-p)))

;;; ement-conf ends here
