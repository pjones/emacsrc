#!/usr/bin/env ruby

################################################################################
# Download the latest PIG magazine and add it to iTunes
require('rss')
require('open-uri')
require('ostruct')
require('optparse')
require('rubygems')
require('nokogiri')

################################################################################
class Downloader
  
  ##############################################################################
  RSS_URL = 'http://www.pigmag.com/it/tag/magazine/feed/'
  
  ##############################################################################
  STATE_FILE = File.expand_path('~/.pigmagpdf')
  
  ##############################################################################
  DOWNLOAD_DIR = File.expand_path('~/Downloads')
  
  ##############################################################################
  ISSUE_REGEX = %r|(\d+)\s*$|
  
  ##############################################################################
  MONTHS = %w(gennaio febbraio marzo aprile maggio giugno 
              luglio agosto settembre ottobre novembre dicembre)
  
  ##############################################################################
  DEFAULT_OPTIONS = {
    :verbose => false,
  }
  
  ##############################################################################
  attr_reader(:options)
  
  ##############################################################################
  def initialize
    @last_issue = last_issue
    @options = OpenStruct.new(DEFAULT_OPTIONS)
    
    OptionParser.new do |p|
      p.on('-h', '--help', 'This message') {$stdout.puts(p); exit}
      p.on('--verbose', 'Enable verbose output') {|v| options.verbose = v}
    end.parse!(ARGV)
  end
  
  ##############################################################################
  def run
    verbose("last issue is #{@last_issue}")

    open(RSS_URL) do |file|
      RSS::Parser.parse(file.read, false)
    end.items.reverse.each {|i| item(i)}

    self.last_issue = @last_issue
    verbose("last issue is #{@last_issue}")
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def last_issue
    File.read(STATE_FILE).to_i rescue 0
  end
  
  ##############################################################################
  def last_issue= (issue)
    File.open(STATE_FILE, 'w') {|f| f.puts(issue.to_s)}
  end
  
  ##############################################################################
  def item (issue)
    verbose("RSS item title: #{issue.title}")
    return unless issue_number = issue.title.match(ISSUE_REGEX)

    issue_number = issue_number[1].to_i
    return unless issue_number > @last_issue

    verbose("fetching #{issue.link}")
    download_url = nil

    page = Nokogiri::HTML(open(issue.link))
    url = page.css('a[href$=pdf]').first['href']
    verbose("issue URL: #{url}")

    file_name = File.join(DOWNLOAD_DIR, "pig-#{issue_number}.pdf")
    File.unlink(file_name) if File.exist?(file_name)
      
    return unless system(*%W(curl -sLo #{file_name} #{url}))
    return unless File.exist?(file_name)
    
    date = issue.pubDate
    month = MONTHS[date.month - 1]
    name = "PIG Magazine #{issue_number} #{month} #{date.year}"

    verbose("adding to iTunes: #{name}")
    add = [File.expand_path('add-to-itunes.rb', File.dirname(__FILE__))]
    add << '--file'     << file_name
    add << '--name'     << name
    add << '--artist'   << 'PIG Mag'
    add << '--album'    << 'PIG Mag'
    add << '--sort'     << 'PIG' + issue_number.to_s.rjust(3, '0')
    add << '--genre'    << 'Fashion & Music'
    add << '--year'     << date.year.to_s
    add << '--playlist' << 'iPad Reading'
    return unless system(*add)
    
    File.unlink(file_name)
    @last_issue = issue_number
  end
  
  ##############################################################################
  def verbose (msg)
    $stdout.puts(msg) if options.verbose
  end
end

################################################################################
begin
  Downloader.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
