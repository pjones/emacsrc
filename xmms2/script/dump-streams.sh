#!/bin/sh

################################################################################
# Dumps all radio stations with their names and urls.
# 
# You can then get just the URLs with:
#
#   awk -F:::: '{print $2}'
#
################################################################################
xmms2 search -o channel -l channel,url 'in:Playlists/Radio' |\
  awk -F'|' 'NR > 2 && ! /----/ {print $1 "::::" $2}' |\
  sed -re 's/[[:space:]]+/ /g'
