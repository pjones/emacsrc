# This script is called by git_sb_info in rc/zsh/app/git.zsh

source ~/.zsh/app/git.zsh
top=$(cd ..; git rev-parse --show-toplevel)
ref=$(git_current_branch)
name=$(pwd | sed "s|$top/||")
state=clean

if [[ ${#ref} -eq 0 ]]; then
  ref="(no branch)"
fi

if git_repo_has_changes; then
  state=dirty
elif git_repo_needs_push; then
  state=push
fi

printf '%15s  %5s  %s\n' $ref $state $name
