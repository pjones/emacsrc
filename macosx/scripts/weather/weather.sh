#!/bin/sh

################################################################################
# This script uses the Weather Underground API:
# http://wiki.wunderground.com/index.php/API_-_XML

################################################################################
if [ $# -ne 0 ]; then
  ZIP=$1
  shift
else
  ZIP=80026
fi

XML=/tmp/weather-$ZIP.xml
CACHE_FOR=3600

################################################################################
NOW=`date +%s`
MTIME=`stat -f %Dm $XML`
AGE=`expr $NOW - ${MTIME:-0}`

if [ \( ! -r $XML \) -o \( ${AGE:-0} -ge $CACHE_FOR \) ]; then
  curl --silent --output $XML \
    'http://api.wunderground.com/auto/wui/geo/ForecastXML/index.xml?query='$ZIP
fi

if [ ! -r $XML ]; then
  echo "$0: failed to download weather XML file"
  exit 1
fi

################################################################################
XSL_FILE=`dirname $0`/weather.xsl
xsltproc $XSL_FILE $XML
