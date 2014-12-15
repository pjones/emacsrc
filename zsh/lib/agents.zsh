# Some variables to always export.
export SSH_AGENT_PID
export SSH_AUTH_SOCK
export GPG_AGENT_INFO

# Only load these files in an GUI session
if [ -n "$DISPLAY" ]; then

  # Load SSH agent environment vars.
  if [ -r ~/.ssh/agent ]; then
      eval `cat ~/.ssh/agent | egrep -v ^echo`
  fi

  # Load GPG agent environment vars.
  if [ -r ~/.gpg-agent-info ]; then
      eval `cat ~/.gpg-agent-info`
  fi
fi
