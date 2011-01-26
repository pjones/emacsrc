#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')
require('ostruct')
require('optparse')

################################################################################
class Inserter
  
  ##############################################################################
  DEFAULT_OPTIONS = {
    'file'   => [nil, '--file=PATH',   'Path of file to add'],        
    'name'   => [nil, '--name=NAME',   'Name of track/file'],         
    'artist' => [nil, '--artist=NAME', 'Artist name'],                
    'album'  => [nil, '--album=NAME',  'Album name'],                 
    'sort'   => [nil, '--sort=VALUE',  'Set the sort name to VALUE'], 
  }
  
  ##############################################################################
  attr_reader(:options)
  
  ##############################################################################
  def initialize
    @options = OpenStruct.new(Hash[DEFAULT_OPTIONS.map {|k,v| [k,v.first]}])

    OptionParser.new do |parser|
      parser.on('-h', '--help', 'This message') do
        $stdout.puts(parser)
        exit
      end
      
      DEFAULT_OPTIONS.keys.sort.each do |k|
        v = DEFAULT_OPTIONS[k]
        parser.on(v[1], v[2]) {|op| options.send("#{k}=", op)}
      end
    end.parse!
  end
  
  ##############################################################################
  def add
    if options.file.nil? or !File.exist?(options.file)
      raise("no file given to add, or file not found")
    end
    
    file_ref = MacTypes::FileURL.path(File.expand_path(options.file))
    itunes   = Appscript.app('iTunes.app')
    track    = itunes.add(file_ref)
    
    track.name.set(options.name)      unless options.name.nil?
    track.artist.set(options.artist)  unless options.artist.nil?
    track.album.set(options.album)    unless options.album.nil?
    track.sort_name.set(options.sort) unless options.sort.nil?
  end
end

################################################################################
begin
  Inserter.new.add
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
