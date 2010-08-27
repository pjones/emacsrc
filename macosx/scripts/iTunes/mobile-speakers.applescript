(*
  Script object to make calling a defined function easier while in the
  iTunes and System Events name space.
*)
script remoteSpeakerFinder
	global the_bwindow, the_state
	
	-- Given a list of buttons, find the remote speakers button
	-- by finding the first button with a name that isn't in a 
	-- rejection list.
	on findCorrectButton(in_buttons)
		set buttons_to_skip to {"Burn Disc"}
		
		repeat with a_button in in_buttons
			try -- some buttons don't have names
				set the_name to name of a_button
				if buttons_to_skip does not contain {the_name} then
					return the_name
				end if
			end try
		end repeat
		
		return 16 -- default response
	end findCorrectButton
	
	on setRemoteSpeakersTo(item_index)
		tell application "System Events"
			tell process "iTunes"
				
				set the_buttons to (get buttons of window 1)
				set the_speaker_button to (remoteSpeakerFinder's findCorrectButton(the_buttons))
				
				-- Switch to the speakers in my bedroom
				set frontmost to true
				click button the_speaker_button of window 1
				key code 115 -- Home Key (first speaker in list)
				
				repeat item_index times
					key code 125 -- Down Arrow
				end repeat
				
				key code 36 -- Return Key
				
				-- Wait for iTunes to connect to the speakers
				delay 5
			end tell
		end tell
	end setRemoteSpeakersTo
	
	on prepareWindow()
		tell application "iTunes"
			set the_bwindow to (get first browser window)
			set the_state to (get minimized of the_bwindow)
			set visible of the_bwindow to true
			set minimized of the_bwindow to false
		end tell
	end prepareWindow
	
	on resetWindow()
		tell application "iTunes"
			-- Return window to previous state
			set minimized of the_bwindow to the_state
		end tell
	end resetWindow
end script

on handle_string(s)
	remoteSpeakerFinder's prepareWindow()
	remoteSpeakerFinder's setRemoteSpeakersTo(s)
	remoteSpeakerFinder's resetWindow()
	set volume 1
	tell application "iTunes"
		play (get the first item of (get every track where database ID is 24759))
	end tell
end handle_string

on run
	remoteSpeakerFinder's prepareWindow()
	remoteSpeakerFinder's setRemoteSpeakersTo(2)
	remoteSpeakerFinder's resetWindow()
end run
