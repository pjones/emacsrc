#!/usr/bin/env ruby

################################################################################
# 
# Given a file that contains file names, move those files to the given
# directory.
#
#  Example:
#
#   ruby safe-move.rb list-of-files.txt /Volumes/AVRAID/Trash
#
################################################################################

raise("read the script dummy") if ARGV.size != 2

$fname = ARGV.first
$dir   = ARGV.last

if !File.exist?($dir) or !File.directory?($dir)
  raise("#{$dir} doesn't exist")
end

File.open($fname) do |file|
  while line = file.gets
    old_name = line.chomp
    new_name = File.join($dir, old_name.gsub('/', '%2f'))
    
    # $stdout.puts("#{old_name} => #{new_name}")

    if !system('mv', old_name, new_name)
      $stderr.puts("ERROR: #{old_name}")
    end
  end
end
