-- Original script from Murphy Mac at http://murphymac.com/itunes-alarm-clock/
-- Start iTunes from cron and slowly bring volume up
tell application "System Events"
  tell process "iTunes"
     -- Switch to the speakers in my bedroom
     set frontmost to true
     click button 15 of window 1 -- Remote Speakers Menu
     key code 115                -- Home Key
     key code 125                -- Down Arrow
     key code 125                -- Down Arrow
     key code 36                 -- Return Key
  end tell
end tell

tell application "iTunes"
  set volume 1
  set sound volume to 1
  play playlist "Wake-Up"
  set x to 0

  repeat until x >= 100
    set x to x + 10
    if x <= 100 then set sound volume to x
    delay 30
  end repeat
end tell
