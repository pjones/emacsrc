tell application "iTunes"
	set i to 0
	if selection is not {} then
		repeat with sel in selection
			set i to (i + 1)
			set sn to (do shell script "printf %04d " & i)
			set the sort name of sel to sn
		end repeat
	end if
end tell