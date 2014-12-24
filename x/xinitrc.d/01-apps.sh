#!/bin/sh

################################################################################
# Some base variables and actions.
uid=`id -u`
ssh_agent_file=$HOME/.ssh/agent
mkdir -p `dirname $ssh_agent_file`

################################################################################
# Start dbus for everything else.
eval `dbus-launch --auto-syntax`

################################################################################
# $1: A string to use with pgrep to find out if it's running.
# $2: The name of a script/app to start.
# $@: Anything else is given to $1.
maybe_start () {
  name=$1; shift
  bin=$1;  shift

  if which $bin > /dev/null 2>&1; then
    if ! pgrep -u $uid `echo $name` > /dev/null 2>&1; then
      $bin "$@"
    fi
  fi
}

################################################################################
start_ssh_agent () {
  # Start SSH Agent
  if [ -z "$SSH_AUTH_SOCK" ]; then
    pkill -u $uid ssh-agent
    ssh-agent > $ssh_agent_file
  else
    echo "SSH_AUTH_SOCK=$SSH_AUTH_SOCK" > $ssh_agent_file
  fi

  chmod 600 $ssh_agent_file
}

################################################################################
start_gpg_agent () {
  # Start GPG Agent
  pkill -u $uid gpg-agent
  gpg-agent \
    --daemon \
    --default-cache-ttl 43200 \
    --max-cache-ttl 86400 \
    --write-env-file > /dev/null
  chmod 600 ~/.gpg-agent-info
}

################################################################################
# Gnome Keyring Daemon acts like ssh-agent and gpg-agent.
start_gnome_keyring () {
  # Kill already running daemon.
  pkill -u $uid gnome-keyring-daemon

  # Start a new one.
  gnome-keyring-daemon -c ssh > $ssh_agent_file
  chmod 0600 $ssh_agent_file
}

################################################################################
# Start authentication daemons.
if which gnome-keyring-daemon > /dev/null 2>&1; then
  start_gnome_keyring
  start_gpg_agent
else
  start_ssh_agent
  start_gpg_agent
fi

################################################################################
# Load agent environment variables.
. ~/.zsh/lib/agents.zsh

################################################################################
# On-screen notifications.  So many to choose from.
if which notify-osd > /dev/null 2>&1; then
    notify-osd &
elif [ -x /usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd ]; then
  /usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd &
else
  /usr/lib/notification-daemon/notification-daemon &
fi

################################################################################
# Start some services.
maybe_start pulseaudio start-pulseaudio-x11
maybe_start mpd mpd
maybe_start tmux ts -cs home
maybe_start "-f 'emacs --daemon'" start-emacs-daemons.sh
maybe_start urxvtd urxvtd -q -o -f
maybe_start bitlbee bitlbee -c $HOME/keys/bitlbee/bitlbee.conf
maybe_start wicd-gtk wicd-gtk -t &
