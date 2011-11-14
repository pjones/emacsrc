zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# prevent CVS directories from being auto completed
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'

# other stuff for cd(1)
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# ignore functions that begin with _
zstyle ':completion:*:functions' ignored-patterns '_*'

autoload -U compinit; compinit
