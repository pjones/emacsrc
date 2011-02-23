#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')
require('ostruct')
require('optparse')

################################################################################
class TrackInfo
  
  ##############################################################################
  attr_reader(:data)
  
  ##############################################################################
  def initialize
    @attributes = %w(track_number track_count sort_name)
    @data = OpenStruct.new
  end
  
  ##############################################################################
  def set (track)
    @attributes.each do |attr|
      if value = @data.send(attr)
        track.send(attr).set(value)
      end
    end
  end
end

################################################################################
class Reorder
  
  ##############################################################################
  attr_reader(:options)
  
  ##############################################################################
  def initialize
    @options = OpenStruct.new
    @itunes  = Appscript.app('iTunes.app')
    @tracks  = @itunes.selection.get
    @info    = @tracks.map {|t| TrackInfo.new}
    
    OptionParser.new do |p|
      p.on('-h', '--help', 'This message') {$stdout.puts(p); exit}
      p.on('-s', '--sort=PREFIX', 'Set the sort name') {|s| options.sort = s}
      p.on('-t', '--track-num', 'Set track numbers') {|t| options.tracknum = t}
    end.permute!(ARGV)
    
    if !options.sort and !options.tracknum
      raise("no operation selected")
    end
    
    raise("nothing slected in itunes") if @tracks.size.zero?
  end
  
  ##############################################################################
  def run
    update_track_numbers if options.tracknum
    update_sort_name     if options.sort
    @tracks.each_with_index {|track, i| @info[i].set(track)}
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def update_track_numbers
    total = @tracks.size
    
    @info.each_with_index do |track, i|
      track.data.track_number = i+1
      track.data.track_count  = total
    end
  end
  
  ##############################################################################
  def update_sort_name
    @info.each_with_index do |track, i|
      track.data.sort_name = options.sort + ' ' + (i+1).to_s.rjust(3, '0')
    end
  end
end

################################################################################
begin
  Reorder.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
