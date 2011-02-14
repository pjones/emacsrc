tell application "iTunes"
  set the_volume to (get sound volume)

  repeat while the_volume > 0
    set i to the_volume - 5
    if i < 0 then set i to 0
    set sound volume to i
    set the_volume to i
    delay 1
  end repeat

  stop
  set sound volume to 90
end tell
