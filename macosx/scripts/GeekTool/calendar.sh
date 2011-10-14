#!/bin/sh

now=`date '+%Y-%m-%d %H:%M %z'`
then=`date '+%Y-%m-%d 23:59 %z'`

icalBuddy -iep title,datetime -ps '|: |' -nc eventsFrom:"$now" to:"$then"
