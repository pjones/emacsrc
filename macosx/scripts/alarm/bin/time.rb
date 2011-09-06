#!/usr/bin/env ruby

################################################################################
$:.unshift(File.expand_path('../lib', File.dirname(__FILE__)))
require('alarm')
require('ostruct')
require('optparse')

################################################################################
class AnnounceTime
  
  ##############################################################################
  DEFAULT_OPTIONS = {
    :stop    => false,                     
    :speaker => Alarm::DEFAULT_SPEAKER, 
  }
  
  ##############################################################################
  attr_reader(:options)
  
  ##############################################################################
  def initialize
    @options = OpenStruct.new(DEFAULT_OPTIONS)
    
    OptionParser.new do |o|
      o.on('-h', '--help', 'This message') {$stderr.puts(o); exit}
      o.on('-s', '--stop', 'Stop iTunes after announcement') {|s| options.stop = s}
      o.on('--speaker=NAME', 'Set the speaker') {|s| options.speaker = s}
    end.parse!(ARGV)

    @airfoil = Alarm::Airfoil.new
    @itunes  = Alarm::ITunes.new
    @say     = Alarm::Say.new(@airfoil)
    @playing = @itunes.playing?
  end
  
  ##############################################################################
  def run
    @airfoil.default_speaker = options.speaker
    @itunes.fade_out_and_stop if @playing
    @say.speak_string("The time is now #{Time.now.strftime("%H:%M").sub(/^0/, '')}")
    
    if @playing and !options.stop
      @airfoil.get_audio_from('iTunes')
      @itunes.fade_in_playlist("Morning Energy", :speed => :fast)
    end
  end
end

################################################################################
begin
  AnnounceTime.new.run
rescue RuntimeError => e
  $stderr.puts($0 + "ERROR: #{e}")
  exit(1)
end

