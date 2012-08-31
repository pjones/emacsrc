#!/bin/sh

# Moves the currently playing song into my "Ban" playlist.
. ~/lib/sh/playlists.sh
mpd_move_current_to_playlist "Ban"
mpc -q del 0
