#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')
require('optparse')
require('ostruct')

################################################################################
class Command
  
  ##############################################################################
  DEFAULT_OPTIONS = {
    :order => %w(Terminal Emacs),
    :width => 1920,
  }
  
  ##############################################################################
  attr_reader(:options)

  ##############################################################################
  def initialize
    @options = OpenStruct.new(DEFAULT_OPTIONS)
    
    OptionParser.new do |p|
      p.on('-h', '--help', 'This message') {$stdout.puts(p); exit}
      p.on('--terminal', 'Activate the terminal') {options.order = %w(Emacs Terminal)}
    end.parse(ARGV)
  end
  
  ##############################################################################
  def run
    se = Appscript.app('System Events')
    
    se.application_processes.get.each do |process|
      next unless process.visible.get
      next if process.windows.get.size.zero?
      name = process.name.get
      next if ['Terminal', 'Emacs'].include?(name)
      hide_application(name, process)
    end
    
    options.order.each do |name|
      Appscript.app(name).activate
    end
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def hide_application (name, process)
    app = Appscript.app(name) rescue nil
    return unless app.respond_to?(:windows)
    
    if app.windows.get.detect {|w| w.bounds.get[0] < options.width}
      process.visible.set(false)
    end
  end
end

################################################################################
begin
  Command.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
