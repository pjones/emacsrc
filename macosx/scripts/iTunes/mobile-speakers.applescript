(*
  Script object to make calling a defined function easier while in the
  iTunes and System Events name space.
*)
script remoteSpeakerFinder
	global the_bwindow, the_state
	
	-- Find the remote speakers button
	on findCorrectButton()
		tell application "System Events"
			tell process "iTunes"
				set in_buttons to (get buttons of window 1)
				
				repeat with a_button in in_buttons
					set the_desc to (get description of a_button)
					if the_desc is equal to "remote speakers" then
						return a_button
					end if
				end repeat
			end tell
		end tell
	end findCorrectButton
	
	on setRemoteSpeakersTo(item_index)
		tell application "System Events"
			tell process "iTunes"
				
				set the_speaker_button to (remoteSpeakerFinder's findCorrectButton())
				
				-- Switch to the speakers in my bedroom
				set frontmost to true
				click the_speaker_button
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
	tell application "iTunes" to stop
	remoteSpeakerFinder's prepareWindow()
	remoteSpeakerFinder's setRemoteSpeakersTo(s)
	remoteSpeakerFinder's resetWindow()
	set volume 1
	tell application "iTunes" to play (get the first item of (get every track where database ID is 24759))
end handle_string

on run
	remoteSpeakerFinder's prepareWindow()
	remoteSpeakerFinder's setRemoteSpeakersTo(2)
	remoteSpeakerFinder's resetWindow()
end run
