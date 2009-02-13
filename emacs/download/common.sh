#!/bin/sh

################################################################################
if [ $# -ne 1 ]; then
  echo "missing version number"
  exit 1
fi

VERSION=$1
PREFIX="${HOME}/Local"
CURL_OPTIONS="--progress-bar"
OSNAME=`uname -s`

SED_OPTIONS="-E"
test $OSNAME = Linux && SED_OPTIONS="-r"

################################################################################
die ()
{
  # $1: the error message
  echo $1
  exit 1
}

################################################################################
fetch_url ()
{
  # $1: the URL to fetch
  # $2: What to call the downloaded file
  # $3: The name of the expected dir after running untar (optional)

  if [ "x$2" != "x" ]; then
    file=$2
  else
    file=`basename $1`
  fi

  if echo $1 | egrep -q '\.git$'; then
    fetch_git_url $1 $file
  else
    test -r $file  || curl ${CURL_OPTIONS} -o $file $1 || die "cURL failure"
    test -r $file  || die "file not downloaded: $file"
    untar $file $3
  fi
}

################################################################################
fetch_git_url ()
{
  # $1: The URL to the Git repo
  # $2: The basename of the repo
  
  gitcache=~/.emacs.d/gitcache/$2
  
  if [ ! -d `dirname $gitcache` ]; then
    mkdir -p `dirname $gitcache` || die "failed to mkdir"
  fi

  if [ -d $gitcache ]; then
    (
      cd $gitcache 
      git checkout master > /dev/null 2>&1
      git pull -q > /dev/stderr || exit 1
    ) || die "git pull failed"
  else
    (
      cd `dirname $gitcache`
      git clone -q $1 $2 > /dev/stderr || exit 1
    ) || die "git clone failed"
  fi
  
  echo $gitcache
}

################################################################################
untar_name ()
{
  echo $1 | sed $SED_OPTIONS 's/\.tar\.gz|\.tgz|\.tar\.bz2|\.zip//'
}

################################################################################
untar ()
{
  # $1: the name of the tarball (should be .tar.gz)
  # $2: the name of the expected directory (optional)
  if [ "x$2" != "x" ]; then
    dir=$2
  else
    dir=`untar_name $1`
  fi
  
  if [ ! -d $dir ]; then
    if echo $1 | egrep -q '\.tar.gz|\.tgz'; then
      tar xzf $1 || die "failed to untar file: $1"
    elif echo $1 | egrep -q '\.zip'; then
      unzip $1 || die "failed to unzip file: $1"
    fi
  fi

  test -d $dir || die "failed to create: $dir from $1"
  echo $dir
}

################################################################################
clean_files ()
{
  # $1: basename to clean
  if echo $1 | grep -q gitcache; then
    : # nothing
  else
    rm -rf ${1}*
  fi
}
