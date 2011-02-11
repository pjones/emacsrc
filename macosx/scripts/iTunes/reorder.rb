#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')

################################################################################
class Reorder
  
  ##############################################################################
  def initialize
    @itunes = Appscript.app('iTunes.app')
    @tracks = @itunes.selection.get
  end
  
  ##############################################################################
  def run
    update_track_numbers
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def update_track_numbers
    total = @tracks.size
    
    # FIXME: loop over the tracks just once if updating other fields
    # like the sort names
    @tracks.each_with_index do |track, i|
      track.track_number.set(i+1)
      track.track_count.set(total)
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
