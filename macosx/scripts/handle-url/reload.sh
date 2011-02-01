#!/bin/sh

APP=`dirname $0`/HandleURL.app
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -v -f $APP
