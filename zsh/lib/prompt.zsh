# Prompt magic
autoload -U colors; colors

if [[ "x$INSIDE_SCRIPT" = "x" ]]; then
    setopt prompt_percent
    setopt prompt_subst
    prompt_newline=$'\n%{\r%}'
    prompt_backtick='\`'

    precmd () 
    { 
        lc=`fc -lnD -1|awk '{print $1}'`
        branch=$(git_current_branch 2> /dev/null)
        prompt="%{$fg[cyan]%}.----(%{$fg[blue]%} %n%{$fg[red]%}@%{$fg[green]%}%m %{$fg[cyan]%})----(%{$fg[magenta]%} %?/$lc %{$fg[cyan]%})----%{$fg[cyan]%}(%{$fg[yellow]%} %2~ %{$fg[cyan]%})"
        
        if [[ ${#branch} -ne 0 ]]; then
          prompt="${prompt}----%{$fg[cyan]%}(%{$fg[red]%} $branch %{$fg[cyan]%})"
        fi

        prompt="${prompt}${prompt_newline}${prompt_backtick}--%{$reset_color%}> "
        PS1=$prompt
    }
else
    PS1="$ "
fi
