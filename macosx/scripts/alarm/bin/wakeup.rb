#!/usr/bin/env ruby

################################################################################
$:.unshift(File.expand_path('../lib', File.dirname(__FILE__)))
require('alarm')

################################################################################
class WakeupAlarm

  ##############################################################################
  def initialize
    @airfoil = Alarm::Airfoil.new
    @itunes = Alarm::ITunes.new
  end
  
  ##############################################################################
  def run
    @itunes.fade_out_and_stop if @itunes.playing?
    @airfoil.get_audio_from('iTunes')
    @itunes.fade_in_playlist("Wake-Up")
  end
end

################################################################################
begin
  WakeupAlarm.new.run
rescue RuntimeError => e
  $stderr.puts($0 + "ERROR: #{e}")
  exit(1)
end
