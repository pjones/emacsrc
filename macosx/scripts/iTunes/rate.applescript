on handle_string(s)
  tell application "iTunes"
    set rating of current track to s * 20
  end tell
end handle_string

on run
  tell application "GrowlTunes"
    show current track
  end tell
end run
