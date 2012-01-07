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
    # Convert opening bracket to opening paren
    [/\[/, '('],

    # Convert closing bracket to closing paren
    [/\]/, ')'],
    
    # Remove (Explicit)
    [/\s*\(\s*Explicit\s*\)/i, ''],
    
    # Remove (Bonus Track), (Bonus Disc), and (With Bonus Video)
    [/\s*\(\s*(With\s+)?Bonus\s+(Track|Disc|Video)(\s+Version)?\s*\)/i, ''],
    
    # Some weird Depeche Mode track names
    [/\s*\(24\/48\s+PCM[^\)]+\)/i, ''],
    
    # Stupid albums with (+digital booklet)
    [/\s*\(\+?digital\s+booklet\s*\)/i, ''],
    
    # Remove (Remastered) or (Remastered 2006)
    [/\s*\(\s*Remastered[^\)]*\)/i, ''],
    
    # Remove (Disc N) crap
    [/\s*\(\s*Disc\s+\d+\s*\)/i, ''],
    
    # WTF? Remove (cc) from a track name
    [/\s*\(cc\)/i, ''],
  ]
  
  ##############################################################################
  attr_reader(:options)
  
  ##############################################################################
  DEFAULT_OPTIONS = {
    :verbose => false,
  }
  
  ##############################################################################
  def initialize
    @options = OpenStruct.new(DEFAULT_OPTIONS)
    @itunes  = Appscript.app('iTunes.app')
    @tracks  = @itunes.selection.get
    
    OptionParser.new do |p|
      p.on('-h', '--help', 'This message') {$stdout.puts(p); exit}
      p.on('--verbose', 'Enable verbose output') {|v| options.verbose = v}
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
    name   = track.name.get
    album  = track.album.get
    before = "#{name} - #{album}"

    RULES.each do |rule|
      name.gsub!(*rule)
      album.gsub!(*rule)
    end
    
    after = "#{name} - #{album}"
    $stdout.puts("#{before} ==> #{after}") if options.verbose

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
