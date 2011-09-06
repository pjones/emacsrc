#!/usr/bin/env ruby

################################################################################
$:.unshift(File.expand_path('../lib', File.dirname(__FILE__)))
require('alarm')

################################################################################
class WeatherAlarm
  
  ##############################################################################
  def initialize
    srand

    @airfoil = Alarm::Airfoil.new
    @itunes = Alarm::ITunes.new
    @playing = @itunes.playing?
    @itunes.fade_out_and_stop if @playing
  end
  
  ##############################################################################
  def run
    @airfoil.default_speaker = ARGV[0] if !ARGV.empty?
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
    long_now  = Time.now.strftime("%A, %B %d")
    short_now = Time.now.strftime("%B %d")
    weekday   = Time.now.strftime("%A")
    greetings = []

    greetings << "Good morning, today is #{long_now}. Your custom
      weather forecast is being retrieved."

    greetings << "Time to wake up, today is #{long_now}. Please stay
      tuned for today's weather report."

    greetings << "It's a beautiful #{weekday} morning.  The weather
      for today, #{short_now}, is being retrieved."

    say = Alarm::Say.new(@airfoil)
    say.speak_string(greetings[rand(greetings.size)].gsub(/\s*\n\s*/, ' '))
    say.speak_weather(80026)
  end
end

################################################################################
begin
  WeatherAlarm.new.run
rescue RuntimeError => e
  $stderr.puts($0 + "ERROR: #{e}")
  exit(1)
end
