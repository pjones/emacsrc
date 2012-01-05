#!/bin/sh

mpc current --format '%title% [[(%artist% / %album%)]|==%name%]' |\
  sed -r -e 's/==([^:-]{,20})[:-]?.*$/(\1)/' -e 's/ *\)/)/g'
