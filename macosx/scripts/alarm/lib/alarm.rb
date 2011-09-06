################################################################################
require('rubygems')
require('appscript')
require('tempfile')
require('fileutils')

###############################################################################
module Alarm
  MAX_VOL  = 75
  TEMP_DIR = File.expand_path('~/Library/Caches/MorningAlarm')
  WEATHER_SCRIPT = File.expand_path("../../weather/weather.sh", File.dirname(__FILE__))
  DEFAULT_SPEAKER = 'Mobile Audio'
end

################################################################################
require('alarm/airfoil')
require('alarm/itunes')
require('alarm/say')
