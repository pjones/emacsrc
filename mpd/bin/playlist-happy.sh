#!/bin/sh

# Moves the currently playing song into my "Happy" playlist.
. ~/lib/sh/playlists.sh
mpd_move_current_to_playlist "Happy"
