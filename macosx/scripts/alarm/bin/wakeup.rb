#!/usr/bin/env ruby

################################################################################
$:.unshift(File.expand_path('../lib', File.dirname(__FILE__)))
require('alarm')

################################################################################
class MorningAlarm
  
  ##############################################################################
  def initialize
    @airfoil = Alarm::Airfoil.new
    @airfoil.default_speaker = 'Mobile Audio'
  end
  
  ##############################################################################
  def run
    good_morning_and_weather
    # @airfoil.get_audio_from('iTunes')
    # @itunes = Alarm::ITunes.new
    # @itunes.fade_in_playlist("Radio")
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def good_morning_and_weather
    greeting = <<-EOD
      Good morning, today is #{Time.now.strftime("%A, %B %d")}.  
      Your custom weather forecast for Lafayette Colorado
      is being downloaded.
    EOD

    @airfoil.speak_string(greeting.gsub(/\s*\n\s*/, ' '))
    @airfoil.speak_weather(80026, :voice => 'Serena')
  end
end

################################################################################
begin
  MorningAlarm.new.run
rescue RuntimeError => e
  $stderr.puts($0 + "ERROR: #{e}")
  exit(1)
end
