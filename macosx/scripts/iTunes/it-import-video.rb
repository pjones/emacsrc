#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('highline')
require('appscript')
require('nokogiri')
require('open-uri')
require('ostruct')
require('yaml')
require('cgi')

################################################################################
class TMDb

  ##############################################################################
  KEY_FILE = File.expand_path('~/.tmdbkey')

  ##############################################################################
  BASE_URL = 'http://api.themoviedb.org/2.1/'

  ##############################################################################
  def initialize
    raise("missing key file: #{KEY_FILE}") unless File.exist?(KEY_FILE)
    @apikey = File.read(KEY_FILE).strip
  end

  ##############################################################################
  def search (q, single_result=true)
    url = make_url('Movie.search', q)
    doc = Nokogiri::XML(open(url))
    matches = doc.xpath('/OpenSearchDescription/movies/movie').map {|m| convert(m)}

    return matches unless single_result
    return details(matches.first.tmdbid)  if matches.size == 1
    raise("no matching movies for: #{q}") if matches.size.zero?

    selected = HighLine.new.choose do |menu|
      menu.prompt = 'Please pick the correct movie: '
      matches.each do |m|
        title = m.name + " (#{m.year}): "
        title += m.description[0, (80 - title.size)]
        menu.choice(title) {m}
      end
    end
    
    details(selected.tmdbid)
  end

  ##############################################################################
  def details (id)
    url = make_url('Movie.getInfo', id)
    doc = Nokogiri::XML(open(url))
    convert(doc.xpath('/OpenSearchDescription/movies/movie').first)
  end

  ##############################################################################
  private

  ##############################################################################
  def make_url (method, *others)
    BASE_URL + [method, 'en', 'xml', @apikey, *others].map {|o| CGI.escape(o)}.join('/')
  end

  ##############################################################################
  def convert (node)
    movie = {
      :tmdbid      => node.xpath('id').first.content,                                        
      :name        => node.xpath('name').first.content,                                      
      :description => node.xpath('overview').first.content,                               
      :posters     => node.xpath('images/image[@type="poster"]').map {|i| i['url']},         
      :genres      => node.xpath('categories/category[@type="genre"]').map {|c| c['name']},  
      :year        => Time.parse(node.xpath('released').first.content).year,                 
      :author      => node.xpath('cast/person[@job="Author"]').map   {|a| a['name']}.join(', '), 
      :director    => node.xpath('cast/person[@job="Director"]').map {|d| d['name']}.join(', '), 
    }
    
    OpenStruct.new(movie)
  end
end

################################################################################
class Driver

  ##############################################################################
  DEFAULT_OPTIONS = {
    :search      => nil,   
    :info_file   => nil,   
    :itunes_sel  => false, 
    :gen_episode => nil,  
  }

  ##############################################################################
  attr_reader(:options)

  ##############################################################################
  def initialize
    @options = OpenStruct.new(DEFAULT_OPTIONS)
    @mode    = :search

    parser = OptionParser.new do |p|
      p.on('-h', '--help', 'This message') do
        $stdout.puts(p)
        exit
      end

      p.on('-s', '--search=TITLE', 'Search mode for movie TITLE') do |s|
        options.search = s
        @mode = :search
      end
      
      p.on('-f', '--file=NAME', 'Import tracks using a YAML file') do |f|
        options.info_file = f
        @mode = :file
      end
      
      p.on('-S', '--selection', 'Operate on the first iTunes selected item') do |s|
        options.itunes_sel = s
      end
      
      p.on('-g', '--generate', 'Generate a YAML import file') do
        @mode = :generate
      end
      
      p.on('--episode=NUM', 'Start episode at NUM when using -g') do |e|
        options.gen_episode = e.to_i
      end
    end.permute!(ARGV)
  end

  ##############################################################################
  def run
    @itunes = Appscript.app('iTunes.app')

    case @mode
      when :search   then search
      when :file     then load_from_file
      when :generate then generate_load_file
      else raise("WTF: you picked an invalid mode")
    end
  end

  ##############################################################################
  private

  ##############################################################################
  def search
    raise("missing search string, use --search") if options.search.nil?
    
    if !options.itunes_sel
      raise("you should give exactly one movie") unless ARGV.size == 1
      raise("no such file: #{ARGV.first}") unless File.exist?(ARGV.first)
    end

    details = TMDb.new.search(options.search)
    $stdout.puts("Title: #{details.name}")

    if options.itunes_sel
      update_track(details, @itunes.selection.get.first)
    else
      import_track(details, ARGV.first)
    end
  end
  
  ##############################################################################
  def load_from_file
    data = YAML.load_file(options.info_file)
    raise("missing 'basic' key") unless data.has_key?('basic')
    raise("missing 'files' key") unless data.has_key?('files')
    
    base = File.dirname(options.info_file)

    data['files'].each do |file|
      file['file'] = File.expand_path(file['file'], base)
      raise("no such file: #{file['file']}") unless File.exist?(file['file'])
    end
    
    data['files'].each do |file|
      info = OpenStruct.new(data['basic'].merge(file))
      $stdout.puts(info.file)
      import_track(info, info.file)
    end
  end
  
  ##############################################################################
  def generate_load_file
    info = {
      'basic' => {
        'show'       => 'Show Name',         
        'album'      => 'Show With Season', 
        'genre'      => 'Comedy',           
        'year'       => 1990,               
        'season'     => 1,                 
        'posters'    => 'artwork.jpg',    
        'video_kind' => 'TV_show',     
      }
    }
    
    files = []
    episode = options.gen_episode
    
    ARGV.each do |name|
      file = {
        'file'        => name,           
        'name'        => 'Track_Name', 
        'comment'     => nil,         
        'description' => nil,     
      }
      
      if episode
        file['episode'] = episode
        episode += 1
      end
      
      files << file
    end
    
    info['files'] = files
    $stdout.puts(info.to_yaml)
  end
  
  ##############################################################################
  def import_track (info, file)
    file_ref = MacTypes::FileURL.path(File.expand_path(file))
    track = @itunes.add(file_ref)
    update_track(info, track)
  end
  
  ##############################################################################
  def update_track (info, track)
    track.name.set(info.name)
    track.show.set(info.show || info.name)
    track.album.set(info.album || info.name)
    track.artist.set(info.artist || '')
    track.composer.set(info.director || '')
    track.year.set(info.year)
    track.genre.set(info.genres.first) if info.genres
    track.genre.set(info.genre) if info.genre
    track.comment.set(info.comment || '')
    track.description.set(info.description || '')
    track.season_number.set(info.season.to_i) if info.season
    track.episode_number.set(info.episode.to_i) if info.episode
    track.video_kind.set(info.video_kind.to_sym) if info.video_kind

    if poster_url = Array(info.posters).detect {|u| u.match(/\.jpg$/i)}
      $stdout.puts("Poster: #{poster_url}")
      data = open(poster_url) {|s| AE::AEDesc.new(KAE::TypeJPEG, s.read)}
      track.artworks[1].data_.set(data)
    end
  end
end

################################################################################
begin
  Driver.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
