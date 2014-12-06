# Prompt magic
autoload -U colors; colors

if [ "x$INSIDE_SCRIPT" = "x" ]; then
  setopt prompt_percent
  setopt prompt_subst
  prompt_newline=$'\n%{\r%}'
  prompt_backtick='\`'

  precmd () {
    # Begin prompt with user@host.
    prompt="%F{cyan}.---(%F{blue}%n%F{red}@%F{green}%m%F{cyan})"

    # Add conditional last process status
    prompt="${prompt}%(?..--[%F{red}%B%?%b%F{cyan}])"

    # Add the current directory two levels deep.
    prompt="${prompt}--%F{cyan}(%F{yellow}%20<..<%2~%<<%F{cyan})"

    # Maybe add in the current Git branch.
    branch=$(git_current_branch 2> /dev/null)

    if [[ ${#branch} -ne 0 ]]; then
      prompt="${prompt}--%F{cyan}(%F{green}%12<..<$branch%<<%F{cyan})"
    fi

    # Move to the next line and present the command prompt.
    prompt="${prompt}${prompt_newline}${prompt_backtick}-%F{white}>%f "
    PS1=$prompt
    RPROMPT=""
  }
else
  PS1="$ "
fi
