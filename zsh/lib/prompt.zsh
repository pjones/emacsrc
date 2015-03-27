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
    prompt="${prompt}%(?..-[%F{red}%B%?%b%F{cyan}])"

    # Add the current directory two levels deep.
    prompt="${prompt}-%F{cyan}(%F{yellow}%20<..<%2~%<<%F{cyan})"

    # Various flags to show.
    flags=""

    # Maybe add in the current Git branch.
    branch=$(git_current_branch 2> /dev/null)

    if [[ ${#branch} -ne 0 ]]; then
      if git_repo_has_changes; then
        color=red
      else
        color=green
      fi

      if [[ $branch == "master" && $color == "green" ]]; then
        flags="${flags}%F{${color}}G"
      else
        prompt="${prompt}-%F{cyan}(%F{${color}}%12<..<$branch%<<%F{cyan})"
      fi
    fi

    # Maybe add info about the current nix-shell.
    if _nix-inside-shell; then
      flags="${flags}%F{magenta}N"
    fi

    # Incorporate flags.
    if [[ -n $flags ]]; then
      prompt="${prompt}-%F{cyan}{${flags}%F{cyan}}"
    fi

    # Move to the next line and present the command prompt.
    prompt="${prompt}${prompt_newline}${prompt_backtick}-%F{white}>%f "
    PS1=$prompt
    RPROMPT=""
  }
else
  PS1="$ "
fi
