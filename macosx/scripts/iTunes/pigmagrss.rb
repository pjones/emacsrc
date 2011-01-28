#!/usr/bin/env ruby

################################################################################
# Download the latest PIG magazine and add it to iTunes
require('rss')
require('open-uri')

################################################################################
class Downloader
  
  ##############################################################################
  RSS_URL = 'http://pigmag.com/it/tag/magazine/feed/'
  
  ##############################################################################
  STATE_FILE = File.expand_path('~/.pigmagpdf')
  
  ##############################################################################
  DOWNLOAD_DIR = File.expand_path('~/Downloads')
  
  ##############################################################################
  DOWNLOAD_REGEX = %r(http://www.sendspace.com/[^"]+) # " # damn syntax highlighting
  
  ##############################################################################
  ISSUE_REGEX = %r|(\d+)\s*$|
  
  ##############################################################################
  MONTHS = %w(gennaio febbraio marzo aprile maggio giugno 
              luglio agosto settembre ottobre novembre dicembre)
  
  ##############################################################################
  def initialize
    @last_issue = last_issue
  end
  
  ##############################################################################
  def run
    open(RSS_URL) do |file|
      RSS::Parser.parse(file.read, false)
    end.items.reverse.each {|i| item(i)}

    self.last_issue = @last_issue
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
    return unless issue_number = issue.title.match(ISSUE_REGEX)
    return unless url = issue.content_encoded.match(DOWNLOAD_REGEX)

    issue_number = issue_number[1].to_i
    return unless issue_number > @last_issue

    file_name = File.join(DOWNLOAD_DIR, "pig-#{issue_number}.pdf")
    File.unlink(file_name) if File.exist?(file_name)
      
    return unless system(*%W(curl -sLo #{file_name} #{url[0]}))
    return unless File.exist?(file_name)
    
    date = issue.pubDate
    month = MONTHS[date.month - 1]
    name = "PIG Magazine #{issue_number} #{month} #{date.year}"

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
end

################################################################################
begin
  Downloader.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
