#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')

################################################################################
class AlbumPlaylist
  
  ##############################################################################
  PLAYLIST_SUFFIX = '(Calculated)'
  
  ##############################################################################
  def initialize
    @itunes = Appscript.app('iTunes.app')
    @input  = @itunes.browser_windows[1].view
  end
  
  ##############################################################################
  def run
    create_output_playlist
    add_tracks_to_output_playlist
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def create_output_playlist
    name = @input.name.get.sub(/\([^)]+\)/, PLAYLIST_SUFFIX)
    name += " #{PLAYLIST_SUFFIX}" if name == @input.name.get
    
    @output =
      if @itunes.playlists[name].exists
        @itunes.playlists[name].get
      else
        @itunes.make(:new => :user_playlist, :with_properties => {:name => name})
      end

    @output.tracks.delete
  end
  
  ##############################################################################
  def add_tracks_to_output_playlist
    @input.tracks.get.each do |track|
      next if track.album_rating_kind.get == :computed
      @itunes.duplicate(track, :to => @output)
    end
  end
end

################################################################################
if $0 == __FILE__
  AlbumPlaylist.new.run
end
