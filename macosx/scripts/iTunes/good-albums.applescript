tell application "iTunes"
	try
		set thisPlaylist to view of front window
		set thisSource to container of thisPlaylist
	on error
		display dialog "You must select a Playlist first." buttons {"Cancel"} default button 1 with icon 2
	end try
	
	set global_album_list to {}
	set new_playlist_name to "Great Albums (Calculated)"
	
	if not (exists playlist new_playlist_name of thisSource) then
		set the_playlist to make new user playlist at thisSource with properties {name:new_playlist_name}
	else
		set the_playlist to user playlist new_playlist_name of thisSource
		delete tracks of the_playlist
	end if
	
	repeat with the_track in (get tracks of thisPlaylist)
		set the_album_rating to (get album rating of the_track)
		set the_album_name to (get album of the_track)
		
		if the_album_name is not "" and the_album_name is not in global_album_list then
			set end of global_album_list to the_album_name
			set the_album_tracks to (tracks of thisPlaylist whose album is the_album_name)
			set the_number_of_tracks to (length of the_album_tracks)
			set number_of_rated_tracks to 0
			
			repeat with track_to_check_rating in the_album_tracks
			  set the_rating to (rating of track_to_check_rating)
			  if the_rating is greater than 0 then
			    set number_of_rated_tracks to (number_of_rated_tracks + 1)
			  end
			end
			
			if the_number_of_tracks is greater than 4 and number_of_rated_tracks is greater than 4 then
				repeat with the_track_to_add in the_album_tracks
					duplicate the_track_to_add to the_playlist
				end repeat
			end if
		end if
	end repeat
end tell