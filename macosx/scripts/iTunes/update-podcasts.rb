#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')
require('set')

################################################################################
seen     = Set.new
itunes   = Appscript.app('iTunes.app')
playlist = itunes.playlists['Podcasts'].get

playlist.tracks.get.each do |track|
  album = track.album.get
  next if seen.include?(album)
  
  seen << album
  itunes.updatePodcast(track)
end
