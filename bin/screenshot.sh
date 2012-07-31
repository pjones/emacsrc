#!/bin/sh

################################################################################
year=`date +%Y`
screenshot_dir=~/documents/pictures/screenshots/$year
screenshot_file=$screenshot_dir/`date +%Y-%m-%d-%H:%M`.png
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
ssh $ssh_host mkdir -p $ssh_path
scp -q $screenshot_file ${ssh_dest}/
echo ${url_base}/`basename $screenshot_file` | xsel -ib
