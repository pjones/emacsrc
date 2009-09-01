-- Original script from Murphy Mac at http://murphymac.com/itunes-alarm-clock/
-- Start iTunes from cron and slowly bring volume up
run script POSIX file "/Users/pjones/Develop/pmade/rc/macosx/scripts/iTunes/mobile-speakers.applescript"

tell application "iTunes"
  set volume 1
  set sound volume to 1
  play playlist "Wake-Up"
  set i to 0
  set n to 70

  repeat until i >= n
    set i to i + 5
    if i <= n then set sound volume to i
    delay 15
  end repeat
end tell
