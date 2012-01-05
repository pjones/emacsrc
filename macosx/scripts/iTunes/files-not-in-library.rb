#!/usr/bin/env ruby

################################################################################
#
# This script will output a list of decoded file names that are in
# your iTunes music library.  This is useful for comparing with the
# output of find(1) to locate files that iTunes doesn't know about, or
# let you know about files that iTunes thinks are in your music
# directory but aren't.
#
################################################################################
require('rubygems')
require('nokogiri')
require('cgi')

################################################################################
LIBRARY_XML = File.expand_path('~/Music/iTunes/iTunes Music Library.xml')
MUSIC_DIR   = '/Volumes/AVRAID/iTunes/Music/'

################################################################################
class Library
  
  ##############################################################################
  def run
    file = File.open(LIBRARY_XML)
    doc = Nokogiri::XML(file)

    # This is the correct xpath, but nokogiri chokes on it and takes
    # an infinite amount of time to process it:
    #
    #   //key[text() = "Location"]/following::string

    doc.xpath('/plist/dict/dict/dict').each do |dict|
      dict.xpath('key[text() = "Location"]').each do |loc|
        url = loc.next_sibling.content.sub('file://localhost', '')
        url = CGI.unescape(url.gsub('+', '%2b'))
        next unless url[0, MUSIC_DIR.size] == MUSIC_DIR
        $stdout.puts(url)
      end
    end
  ensure
    file.close if file
  end
end

################################################################################
begin
  Library.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
