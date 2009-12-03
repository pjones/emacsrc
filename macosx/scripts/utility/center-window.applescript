script screenHelper
	on screenWidth()
		(do shell script Â
			"defaults read /Library/Preferences/com.apple.windowserver|grep -w Width|head -1|sed -E 's/^[[:space:]]*Width = ([0-9]+).*/\\1/'") Â
			as number
	end screenWidth
	
	on frontMostProcessName()
		tell application "System Events"
			set myFrontMost to name of first item of (processes whose frontmost is true)
			return myFrontMost
		end tell
	end frontMostProcessName
end script



set theFrontMostProcessName to (screenHelper's frontMostProcessName())

tell application theFrontMostProcessName
	set theScreenWidth to (screenHelper's screenWidth())
	set windowSize to bounds of window 1
	set windowXl to item 1 of windowSize
	set windowYt to item 2 of windowSize
	set windowXr to item 3 of windowSize
	set windowYb to item 4 of windowSize
	set windowWidth to windowXr - windowXl
	set bounds of window 1 to {(theScreenWidth - windowWidth) / 2.0, windowYt, (theScreenWidth + windowWidth) / 2.0, windowYb}
end tell
