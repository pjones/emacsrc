#!/usr/bin/env ruby

SIMPLE_SUBS = [
  [/^#\+title:/,  '%'],
  [/^#\+author:/, '%'],
]

SKIP_LINES = [
  /#\+BEGIN_SRC/,
  /#\+END_SRC/,
]

$links = []

while line = gets() do
  next if SKIP_LINES.any? {|re| line.match(re)}
  SIMPLE_SUBS.each {|s| line.gsub!(*s)}
  
  # Update headings
  line.sub!(/^\*+/) {|stars| '#' * stars.length}
  line.sub!(/^(\s*)-/, '\1*')
  
  # Inline code
  line.gsub!(/=([^=]+)=/, '`\1`')
  
  # Update links
  line.gsub!(/\[\[([^\]]+)\]\[([^\]]+)\]\]/) do |match|
    $links << $1
    "[#{$2}] [#{$links.size}]"
  end
  
  $stdout.print(line)
end

$stdout.puts # blank line

$links.each_with_index do |link, index|
  $stdout.puts("[#{index + 1}]: #{link}")
end
