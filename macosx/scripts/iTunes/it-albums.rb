#!/usr/bin/env ruby

################################################################################
#
# The main purpose of this script is to create a new playlist and
# populate it with your favorite albums.  It does this by taking an
# input playlist that should have whole albums in it, and then copying
# them into a playlist if they have a real album rating (manually set
# vs. computed by iTunes).
#
# Before running this script, select an input playlist that contains
# whole albums that are candidates for the output playlist.
#
# For example, create a smart playlist with these rules:
#
#  1. Album Rating is greater than 3 stars
#  2. Media Kind is Music
#
# Select this playlist in iTunes and run this script.  It will weed
# out albums where iTunes computed the album rating based on the
# ratings of its tracks, which I find to be faulty because if you only
# rate a single song (or you have only one song from an album) the
# album rating will be the same as the song rating.
#
# If your input playlist is named "Foo Bar" a new playlist will be
# created called "Foo Bar (Calculated)".  If the input playlist is
# named "Foo Bar (Something in parentheses)" the new playlist will be
# called "Foo Bar (Calculated)".
#
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
