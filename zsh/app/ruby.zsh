################################################################################
# If the rbenv tool is installed, use it.
if [ -d ~/.rbenv/bin ]; then
  path=(~/.rbenv/bin $path)
  eval "$(rbenv init -)"
fi

################################################################################
alias irb='irb --readline -r irb/completion'

################################################################################
rtags () { command rtags --quiet $(find . -type f -name '*.rb')}
