#!/usr/bin/env ruby

################################################################################
$:.unshift(File.expand_path('../lib', File.dirname(__FILE__)))
require('alarm')

################################################################################
class StopAllAlarm
  
  ##############################################################################
  def initialize
    @airfoil = Alarm::Airfoil.new
    @itunes = Alarm::ITunes.new
  end
  
  ##############################################################################
  def run
    @itunes.fade_out_and_stop if @itunes.playing?
    @airfoil.disconnect_all_speakers
    @airfoil.quit
    @itunes.volume = Alarm::MAX_VOL
  end
end

################################################################################
begin
  StopAllAlarm.new.run
rescue RuntimeError => e
  $stderr.puts($0 + "ERROR: #{e}")
  exit(1)
end
