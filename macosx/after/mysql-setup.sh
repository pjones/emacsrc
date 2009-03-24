#!/bin/sh

MYSQL_VER=5

sudo launchctl load -w /Library/LaunchDaemons/org.macports.mysql${MYSQL_VER}.plist
sudo -u mysql mysql_install_db${MYSQL_VER}
