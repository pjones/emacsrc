#!/bin/sh

# Global UI changes
defaults write -g NSRepeatCountBinding -string "^u"
defaults write -g NSNavPanelExpandedStateForSaveMode -bool TRUE
defaults write -g CheckSpellingWhileTyping -bool TRUE
defaults write -g PMPrintingExpandedStateForPrint -bool TRUE

# Force Safari to always use tabs, no matter what!
defaults write com.apple.Safari TargetedClicksCreateTabs -bool TRUE

# The Mac OS X dock application is stupid
defaults write com.apple.dock static-only -bool TRUE
defaults write com.apple.dock autohide -bool TRUE
defaults write com.apple.dock tilesize -int 16
killall Dock
