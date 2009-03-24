#!/usr/bin/env ruby

PASS_THROUGH = %w(PATH MANPATH EDITOR)
DIRECTORY = File.expand_path('~/.MacOSX')
FILENAME  = File.join(DIRECTORY, 'environment.plist')
Dir.mkdir(DIRECTORY) unless File.exist?(DIRECTORY)

HEADER = <<EOT
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
EOT

File.open(FILENAME, 'w') do |file|
  file.puts(HEADER)
  
  ENV.each do |name, value|
    next unless PASS_THROUGH.include?(name)
    file.puts("<key>#{name}</key>")
    file.puts("<string><![CDATA[#{value}]]></string>")
  end
  
  file.puts('</dict></plist>')
end

$stdout.puts("You must now log-out for these changes to take affect.")
