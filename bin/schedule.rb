#!/usr/bin/env ruby -w

################################################################################
#
# This script reads a YAML file with my daily schedule and does a ton
# of stuff with it.
#
################################################################################
require('yaml')
require('time')
require('ostruct')
require('optparse')

################################################################################
class Period
  
  ##############################################################################
  TIME_FORMAT = '%H:%M'
  
  ##############################################################################
  attr_accessor(:start, :stop, :announce, :tasks, :alerted)
  
  ##############################################################################
  def initialize (attributes={})
    @tasks      = Array(attributes['tasks'])
    @announce   = attributes['announce']
    @announce ||= @tasks.first
    @alerted    = false
    
    time = attributes['time']
    
    if !(m = time.match(/^(\d{2}:\d{2})\s+-\s+(\d{2}:\d{2})$/))
      raise("invalid time spec: #{time}")
    end
    
    today  = Time.now.strftime('%Y-%m-%d ')
    @start = Time.parse(today + m[1])
    @stop  = Time.parse(today + m[2])
  end
  
  ##############################################################################
  def due?
    now = Time.now
    !@alerted and (start > now) and (start < (now + 60))
  end
  
  ##############################################################################
  def alert
    message = tasks.map {|t| "- #{t}"}.join("\n")
    system('say', announce)
    system(*%W(growlnotify -m #{message} -n Schedule -t #{announce}))
    @alerted = true
  end
  
  ##############################################################################
  def to_org
    "* #{start.strftime(TIME_FORMAT)}-#{stop.strftime(TIME_FORMAT)} (#{announce})\n" +
      tasks.map {|t| "  - #{t}\n"}.join
  end
end

################################################################################
class Schedule
  
  ##############################################################################
  attr_reader(:name, :periods)
  
  ##############################################################################
  def initialize (name, periods)
    @name    = name
    @periods = periods.map {|p| Period.new(p)}
  end
  
  ##############################################################################
  def valid?
    last_stop = periods.first.start

    periods.each do |period|
      if period.start != last_stop
        msg = "gap in time for #{name} at #{period.start} "
        msg << "expected #{last_stop}"
        raise(msg)
      end

      last_stop = period.stop
    end
  end
  
  ##############################################################################
  def to_org
    periods.map {|p| p.to_org}.join
  end
  
  ##############################################################################
  def next_period (wrap=true)
    now = Time.now
    
    periods.each_with_index do |period, index|
      return index if period.start > now
    end
    
    wrap ? 0 : nil
  end
end

################################################################################
class Driver
  
  ##############################################################################
  DEFAULT_OPTIONS = {
    :orgout => false,
    :names  => false,
    :next   => false,
    :alert  => false,
  }
  
  ##############################################################################
  attr_reader(:options)
  
  ##############################################################################
  def initialize
    @schedules = {}
    @options   = OpenStruct.new(DEFAULT_OPTIONS)
    
    parser = OptionParser.new do |p|
      p.banner = 'Usage: schedule [options] file name'
      p.on('-h', '--help', 'This message') {$stdout.puts(p); exit}
      p.on('--orgout', 'Output orgmode schedule') {options.orgout = true}
      p.on('--names', 'Output a list of names') {options.names = true}
      p.on('--next', 'Output the next period') {options.next = true}
      p.on('--alert', 'Go into alert mode') {options.alert = true}
    end
    
    argv = parser.permute!(ARGV)
    raise("must at least give a file name") if argv.size.zero?
    
    if argv.size != 2 and !options.names
      raise("give file and name")
    end
    
    @file, @name = argv
    parse_file

    if !options.names
      raise("no such schedule: #{@name}") unless @schedules.has_key?(@name)
      @active = @schedules[@name]
    end
  end
  
  ##############################################################################
  def run
    if options.names
      $stdout.puts(@schedules.keys.sort.join(' '))
    elsif options.orgout
      $stdout.puts(@active.to_org)
    elsif options.next
      $stdout.puts(@active.periods[@active.next_period].to_org)
    elsif options.alert
      alert_mode
    end
  end
  
  ##############################################################################
  private
  
  ##############################################################################
  def parse_file
    YAML.load_file(@file).each do |name, periods|
      next unless periods.is_a?(Array)
      @schedules[name] = Schedule.new(name, periods)
      @schedules[name].valid?
    end
  end
  
  ##############################################################################
  def alert_mode
    while i = @active.next_period(false)
      period = @active.periods[i]
      period.alert if period.due?
      sleep(45)
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
