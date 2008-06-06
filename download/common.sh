#!/bin/sh

################################################################################
if [ $# -ne 1 ]; then
  echo "missing version number"
  exit 1
fi

VERSION=$1
PREFIX="${HOME}/Local"
CURL_OPTIONS="-O --progress-bar"

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
  file=`basename $1`
  test -r $file  || curl ${CURL_OPTIONS} $1 || die "cURL failure"
  test -r $file  || die "file not downloaded: $file"
  echo $file
}

################################################################################
untar_name ()
{
  echo $1 | sed -E 's/\.tar\.gz|\.tgz|\.tar\.bz2|\.zip//'
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
  rm -rf ${1}*
}
