to add_track_to_playlist(playlist_name)
	tell application "iTunes"
		set the_track_id to database ID of current track
		
		if not (exists playlist playlist_name) then
			error "Can't find a playlist with the name: \"" & playlist_name & "\""
		end if
		
		if not (exists (some track of playlist playlist_name whose database ID is the_track_id)) then
			duplicate current track to playlist playlist_name
		end if
	end tell

		tell application "GrowlTunes"
			show current track
		end tell
end add_track_to_playlist

on run
	tell me to add_track_to_playlist("Happy")
end run

on handle_string(s)
	tell me to add_track_to_playlist(s)
end handle_string