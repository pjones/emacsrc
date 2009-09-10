(*
  Script object to make calling a defined function easier while in the
  iTunes and System Events name space.
*)
script remoteSpeakerFinder
  -- Given a list of buttons, find the remote speakers button
  -- by finding the first button with a name that isn't in a 
  -- rejection list.
  on findCorrectButton (in_buttons)
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
end script

(*
  Tell iTunes to use the "Mobile" set of remote speakers.
*)
tell application "System Events"
  tell process "iTunes"
     set the_buttons to (get buttons of window 1)
     set the_speaker_button to (remoteSpeakerFinder's findCorrectButton(the_buttons))

     -- Switch to the speakers in my bedroom
     set frontmost to true
     click button the_speaker_button of window 1
     key code 115                -- Home Key (first speaker in list)
     key code 125                -- Down Arrow
     key code 125                -- Down Arrow
     key code 36                 -- Return Key

     -- Wait for iTunes to connect to the speakers
     delay 5
  end tell
end tell
