tell application "Mail"
  activate
  set sig_text to (do shell script "echo '-- ' && cat ~/.comm-sync/etc/signatures/pmade")
  -- make new signature with properties {name:"Default", content:sig_text}
  set content of signatures to sig_text  

  repeat with the_acct in every account
    -- set selected signature to "Default"
  end repeat

  -- set default message format to plain
  set choose signature when composing to true
end tell
