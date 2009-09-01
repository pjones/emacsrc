-- Original script from Murphy Mac at http://murphymac.com/itunes-alarm-clock/
-- Start iTunes from cron and slowly bring volume up

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
