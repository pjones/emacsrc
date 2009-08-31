-- Place the release date of a podcast into its name
tell application "iTunes"
  activate
  repeat with this_track in selection
    set the_orig_name to (get name of this_track)
    set the_release_date to (get release date of this_track)
    set the_date_string to (short date string of the_release_date)
    set name of this_track to (the_date_string & ": " & the_orig_name)
  end repeat
end tell
