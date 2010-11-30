script remoteSpeakerFinder
	global the_bwindow, the_state
	
	-- Find the remote speakers button
	on findCorrectButton()
		tell application "System Events"
			tell process "iTunes"
				set the_window to (get window "iTunes")
				set in_buttons to (get buttons of the_window)
				
				repeat with a_button in in_buttons
					set the_desc to (get description of a_button)
					if the_desc contains "remote speakers" then
						return a_button
					end if
				end repeat
			end tell
		end tell
	end findCorrectButton
	
	on makeSpeakerWindowShow()
		tell application "System Events"
			tell process "iTunes"
				set the_speaker_button to (remoteSpeakerFinder's findCorrectButton())
				
				-- Switch to the speakers in my bedroom
				set frontmost to true
				click the_speaker_button
				key code 119 -- Key Code for "END"
				key code 76 -- Click "enter" for last menu item
				delay 2
			end tell
		end tell
	end makeSpeakerWindowShow
	
	on setValueOfComputerSpeaker(value_to_set)
		makeSpeakerWindowShow()
		
		tell application "System Events"
			tell process "iTunes"
				tell window "Multiple Speakers"
					activate
					
					set the_cbox to (get checkbox 1 of group 1 of row 1 of table 1 of scroll area 1)
					set the_value to (get value of the_cbox as integer)
					
					if the_value is not equal to value_to_set then
						set value of the_cbox to value_to_set
					end if
				end tell
			end tell
		end tell
		
		tell application "iTunes" to close window "Multiple Speakers"
	end setValueOfComputerSpeaker
	
	on turnOffAllRemoteSpeakers()
		makeSpeakerWindowShow()
		
		tell application "System Events"
			tell process "iTunes"
				tell window "Multiple Speakers"
					activate
					
					repeat with a_row in (get rows of table 1 of scroll area 1)
						set the_desc to (get description of checkbox 1 of group 1 of a_row)
						set the_value to (get value of checkbox 1 of group 1 of a_row as integer)
						if the_desc is not equal to "Computer" and the_value is not equal to 0 then
							set value of checkbox 1 of group 1 of a_row to 0
						end if
					end repeat
				end tell
			end tell
		end tell
		
		tell application "iTunes" to close window "Multiple Speakers"
	end turnOffAllRemoteSpeakers
	
	on turnOnGivenSpeakerName(speaker_name)
		makeSpeakerWindowShow()
		
		tell application "System Events"
			tell process "iTunes"
				tell window "Multiple Speakers"
					activate
					
					repeat with a_row in (get rows of table 1 of scroll area 1)
						set the_desc to (get description of checkbox 1 of group 1 of a_row)
						set the_value to (get value of checkbox 1 of group 1 of a_row as integer)
						if the_desc is equal to speaker_name and the_value is not equal to 1 then
							set value of checkbox 1 of group 1 of a_row to 0
							delay 5
						end if
					end repeat
				end tell
			end tell
		end tell
		tell application "iTunes" to close window "Multiple Speakers"
	end turnOnGivenSpeakerName
	
	on setRemoteSpeakersTo(speaker_name)
		makeSpeakerWindowShow()
		
		tell application "System Events"
			tell process "iTunes"
				
				-- Flip the right checkboxes
				tell (get window "Multiple Speakers")
					activate
					tell table 1 of scroll area 1
						activate
						
						set did_activate_correct_speaker to 0
						set computer_speaker to null
						
						repeat with i from 1 to (count of every row)
							tell group 1 of row i
								activate
								
								set the_desc to (description of checkbox 1)
								set the_value to (value of checkbox 1 as integer)
								
								if the_desc = "Computer" then
									set computer_speaker to (get checkbox 1)
									if the_value = 0 then
										click checkbox 1
										delay 10
									end if
								else if the_desc = speaker_name then
									set did_activate_correct_speaker to 1
									if the_value = 0 then
										click checkbox 1
										delay 10
									end if
								else if the_value = 1 then
									click checkbox 1
									delay 10
								end if
							end tell
						end repeat
						
						-- Turn off the computer speakers
						if did_activate_correct_speaker = 1 then
							click computer_speaker
							delay 10
						end if
					end tell
				end tell
			end tell
		end tell
		
		tell application "iTunes" to close window "Multiple Speakers"
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

on run (argv)
	tell application "iTunes" to stop
	set speaker_name to (get item 1 of argv)
	
	remoteSpeakerFinder's prepareWindow()
	remoteSpeakerFinder's setValueOfComputerSpeaker(1)
	remoteSpeakerFinder's turnOffAllRemoteSpeakers()
		
	if speaker_name is not equal to "Computer" then
		remoteSpeakerFinder's turnOnGivenSpeakerName(speaker_name)
		remoteSpeakerFinder's setValueOfComputerSpeaker(0)
	end if
	
	remoteSpeakerFinder's resetWindow()
end run
