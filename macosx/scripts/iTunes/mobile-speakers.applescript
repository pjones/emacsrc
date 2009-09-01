script remoteSpeakerFinder
  on findCorrectButton (in_buttons)
    set buttons_to_skip to {"button", "Burn"}

    repeat with a_button in in_buttons
      set the_name to (word 1 in a_button)
      if buttons_to_skip contains {the_name} then
        return a_button
      end if
    end repeat

    return 16
  end findCorrectButton
end script

tell application "System Events"
  tell process "iTunes"
     set the_buttons to (get buttons of window 1)
     set the_speaker_button to (remoteSpeakerFinder's findCorrectButton(the_buttons))

     (*
     -- Switch to the speakers in my bedroom
     set frontmost to true
     click button the_speaker_button of window 1
     key code 115                -- Home Key
     key code 125                -- Down Arrow
     key code 125                -- Down Arrow
     key code 36                 -- Return Key
     *)
  end tell
end tell
