#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')

################################################################################
class AlbumMaker
  
  ##############################################################################
  def initialize
    @itunes = Appscript.app('iTunes.app')
  end
  
  ##############################################################################
  def run
    playlist = @itunes.browser_windows[1].view.get
    puts playlist.inspect
  end
end

################################################################################
begin
  AlbumMaker.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
