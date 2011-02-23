#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('highline')
require('appscript')
require('nokogiri')
require('open-uri')
require('ostruct')
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
    :search     => nil,       
    :itunes_sel => false, 
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
      
      p.on('-S', '--selection', 'Operate on the first iTunes selected item') do |s|
        options.itunes_sel = s
      end
    end.permute!(ARGV)
  end

  ##############################################################################
  def run
    @itunes = Appscript.app('iTunes.app')

    case @mode
      when :search then search
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
      import_movie(details, ARGV.first)
    end
  end
  
  ##############################################################################
  def import_movie (movie, file)
    file_ref = MacTypes::FileURL.path(File.expand_path(file))
    track = @itunes.add(file_ref)
    update_track(movie, track)
  end
  
  ##############################################################################
  def update_track (movie, track)
    track.name.set(movie.name)
    track.show.set(movie.name)
    track.album.set(movie.album || movie.name)
    track.artist.set(movie.artist || '')
    track.composer.set(movie.director || '')
    track.genre.set(movie.genres.first)
    track.year.set(movie.year)
    track.description.set(movie.description || '')
    
    if poster_url = Array(movie.posters).detect {|u| u.match(/\.jpg$/i)}
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
