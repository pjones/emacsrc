#!/bin/sh

################################################################################
year=`date +%Y`
screenshot_dir=~/documents/pictures/screenshots/$year
screenshot_file=$screenshot_dir/`date +%Y-%m-%d-%H%M`.png
ssh_host=dracula.pmade.com
ssh_path=websites/pmade.com/www/static/images/$year
ssh_dest=${ssh_host}:${ssh_path}
url_base="http://www.pmade.com/static/images/$year"

################################################################################
case "$1" in
  root)
    window=root
    ;;

  window|*)
    window=`xdotool getwindowfocus -f`
    ;;
esac

################################################################################
mkdir -p $screenshot_dir
import -window $window $screenshot_file

################################################################################
remote_name=`md5sum $screenshot_file | cut -f1 -d' '`.png
ssh $ssh_host mkdir -p $ssh_path
scp -q $screenshot_file ${ssh_dest}/$remote_name
echo ${url_base}/$remote_name | xsel -ib
