################################################################################
# Git Log
glog ()
{
    git log --pretty=format:'%Cgreen%h%Creset %Cred%ci%Creset %Cblue%ae%Creset %s' --graph $@
}

################################################################################
# Resolve conflicts by keeping the local changes and throwing away the
# remote changes
git_resolve_with_local ()
{
  if [ $# -eq 0 ]; then
    FILES=`git ls-files -u | awk '{if ($3==1) print $4;}'`
    test -z "$FILES" && exit
  else
    FILES=$*
  fi

  PREFIX=`git rev-parse --show-prefix`

  # need the echo below because the files are someone all glued
  # together in a single string
  for f in `echo $FILES`; do
    echo "--> Tossing remote changes for $f"
    git cat-file blob ":2:${PREFIX}${f}" >| "$f" 2> /dev/null
    touch "$f"
    git add -- "$f"
  done

  echo "now run git commit"
}

################################################################################
# Find out if a repo has uncommitted changes
git_repo_has_changes ()
{
  if ! git status|grep -q -F 'working directory clean'; then
    return 0
  else
    return 1
  fi
}

################################################################################
# Find out if a repo has changes that haven't been pushed
git_repo_needs_push ()
{
  if git status|head -2|grep -q 'ahead of'; then
    return 0
  else
    return 1
  fi
}

################################################################################
# Returns the name of the current git branch if we're in a git repo
git_current_branch ()
{
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

################################################################################
# Create a new branch, and push it to the origin server
git_mk_branch ()
{
  if [ $# -eq 0 ]; then
    echo "Usage: $0 branch [remote]"
    return 1
  fi

  branch=$1
  remote=${2:-origin}

  git checkout -b $branch || return 1
  git config --add branch.${branch}.remote $remote || return 1
  git push $remote $branch || return 1
}

################################################################################
# Delete a branch: FIXME: remove from all remotes
git_rm_branch ()
{
  if [ $# -eq 0 ]; then
    echo "Usage: $0 branch"
    return 1
  fi

  branch=$1
  git branch -d $branch || return 1

  if git branch -a | grep -q origin/$branch; then
    git push origin :heads/$branch
  fi
}

################################################################################
# Pull master, and return to the current branch
git_master_pull ()
{
  remote=origin
  test $# -eq 1 && remote=$1

  branch=`git_current_branch`
  echo "==> Leaving branch ${branch}"
  git checkout master  || return 1
  git pull $remote     || return 1
  git checkout $branch || return 1
}

################################################################################
# List info about each sub-module
function git-sb-info ()
{
  git submodule --quiet foreach 'zsh ~/.zsh/util/git-sb-info.zsh'
}

################################################################################
# Create a new remote repo
git_new_repo ()
{
  remote_path=$1
  remote_name=origin
  remote_host=dracula.pmade.com

  test $# -gt 1 && remote_name=$2
  test $# -gt 2 && remote_host=$3

  ssh $remote_host "mkdir -p $remote_path && cd $remote_path && git init --bare"
  git remote add $remote_name ${remote_host}:$remote_path
  git config --add branch.master.remote $remote_name
  git config --add branch.master.merge refs/heads/master
  git push $remote_name master
}

################################################################################
# Find out about the current repo.
function git-what ()
{
  git describe --long --tags --dirty --always
}
