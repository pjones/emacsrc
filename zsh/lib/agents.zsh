# Load SSH agent environment vars.
if [ -r ~/.ssh/agent ]; then
  eval `cat ~/.ssh/agent | egrep -v ^echo`
  export SSH_AGENT_PID
  export SSH_AUTH_SOCK
fi

# Load GPG agent environment vars.
if [ -r ~/.gpg-agent-info ]; then
  eval `cat ~/.gpg-agent-info`
  export GPG_AGENT_INFO
fi
