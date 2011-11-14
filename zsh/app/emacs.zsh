# Set our EDITOR
if [ -x ~/bin/e ] && which emacsclient > /dev/null 2>&1; then
  export EDITOR="e --wait"
elif which emacsclient > /dev/null 2>&1; then
  export EDITOR="emacsclient"
elif which vim > /dev/null 2>&1; then
  export EDITOR=vim
else
  export EDITOR=vi
fi
