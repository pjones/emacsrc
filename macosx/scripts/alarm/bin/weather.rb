#!/usr/bin/env ruby

################################################################################
$:.unshift(File.expand_path('../lib', File.dirname(__FILE__)))
require('alarm')

################################################################################
class WeatherAlarm
  
  ##############################################################################
  def initialize
    @airfoil = Alarm::Airfoil.new
    @itunes = Alarm::ITunes.new
    @playing = @itunes.playing?
    @itunes.fade_out_and_stop if @playing
  end
  
  ##############################################################################
  def run
    good_morning_and_weather

    if @playing
      @airfoil.get_audio_from('iTunes')
      @itunes.fade_in_playlist("Morning Energy", :speed => :fast)
    end
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

    say = Alarm::Say.new(@airfoil)
    say.speak_string(greeting.gsub(/\s*\n\s*/, ' '))
    say.speak_weather(80026, :voice => 'Serena')
  end
end

################################################################################
begin
  WeatherAlarm.new.run
rescue RuntimeError => e
  $stderr.puts($0 + "ERROR: #{e}")
  exit(1)
end
