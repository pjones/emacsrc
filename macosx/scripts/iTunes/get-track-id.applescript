tell application "iTunes"
	set the_id to (get database ID of (get the first item of the selection))
	display dialog "Selected item ID is " & the_id & ". It's now on the clipboard."
	set the clipboard to "" & the_id
end tell
