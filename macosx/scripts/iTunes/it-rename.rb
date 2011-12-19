#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')
require('ostruct')
require('optparse')

################################################################################
class Rename
  
  ##############################################################################
  RULES = [
    [/\s*\[Explicit\]\s*/i, ''],
  ]
  
  ##############################################################################
  attr_reader(:options)
  
  ##############################################################################
  def initialize
    @options = OpenStruct.new
    @itunes  = Appscript.app('iTunes.app')
    @tracks  = @itunes.selection.get
    
    OptionParser.new do |p|
      p.on('-h', '--help', 'This message') {$stdout.puts(p); exit}
    end.permute!(ARGV)
    
    raise("nothing selected in itunes") if @tracks.size.zero?
  end

  ##############################################################################
  def run
    @tracks.each {|t| update_track(t)}
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def update_track (track)
    name  = track.name.get
    album = track.album.get
    
    RULES.each do |rule|
      name.gsub!(*rule)
      album.gsub!(*rule)
    end
    
    track.name.set(name)
    track.album.set(album)
  end
end

################################################################################
begin
  Rename.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
