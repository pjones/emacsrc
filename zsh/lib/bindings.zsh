# fix ^w
export WORDCHARS="*?_-.[]~={}<>"
autoload backward-kill-word-match select-word-style match-words-by-style
zle -N backward-kill-word-match
select-word-style normal
match-words-by-style normal

# my widgets
for widget in last-cmd-as-expansion; do
    autoload $widget
    zle -N $widget
done

# default key binding is emacs
bindkey -e

# Edit command lines in EDITOR.
autoload edit-command-line
zle -N edit-command-line

# Insert some custom keybindings
bindkey -e ^xe edit-command-line
bindkey -e ^xp push-line-or-edit
bindkey -e ^xh run-help
bindkey -e ^r  history-incremental-search-backward
bindkey -e ^w  backward-kill-word-match
bindkey -e ^o  last-cmd-as-expansion
bindkey -e ^i  expand-or-complete-prefix
