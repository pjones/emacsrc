#!/usr/bin/env ruby

################################################################################
#
# Create a directory structure and symbolic links that can be synced
# to a device via rsync.  The output directory structure will contain
# only tracks mentioned in the input playlists.
#
# Playlists will also be written into the output directory structure.
# It's assumed that the file names in the playlists are relative to
# the music directory.  The generated playlists will contain absolute
# path names for the device you are syncing to.
#
################################################################################
require('digest')
require('fileutils')
require('optparse')
require('ostruct')

################################################################################
class Playlist

  ##############################################################################
  attr_reader(:items)

  ##############################################################################
  def self.read (file_name)
    File.open(file_name) {|f| new(f)}
  end

  ##############################################################################
  def initialize (file)
    @items = []

    while line = file.gets
      clean = line.strip

      next if clean.match(/^\s*$/)
      next if clean.match(/^\s*#/)
      @items << clean
    end
  end
end

################################################################################
class Sync

  ##############################################################################
  DEFAULT_OPTIONS = {
    :device       => nil,
    :music_in     => '~/documents/music',
    :music_out    => 'Music',
    :music_prefix => '/storage/emulated/0/Music/',
    :playlist_in  => '~/sync/%s/playlists',
    :playlist_out => 'Playlists',
    :sync_out     => '~/sync/%s/',
    :verbose      => false,
    :versions_dir => 'versions',
  }

  ##############################################################################
  attr_reader(:options)

  ##############################################################################
  def initialize
    @options = OpenStruct.new(DEFAULT_OPTIONS)

    OptionParser.new do |p|
      p.on('-h', '--help', 'This message') do
        puts(p)
        exit
      end

      p.on('--[no-]verbose', 'Turn on/off verbosity') do |x|
        options.verbose = x
      end

      p.on('-d', '--device=NAME', 'Device to sync') do |x|
        options.device = x
      end

      p.on('-m', '--music-in=DIR', 'Music directory') do |x|
        options.music_in = x
      end

      p.on('-s', '--sync-out=DIR', 'Sync directory') do |x|
        options.sync_out = x
      end

      p.on('-p', '--playlist-in=DIR', 'Playlist directory') do |x|
        options.playlist_in = x
      end
    end.parse!(ARGV)

    raise("missing --device option") if options.device.nil?
  end

  ##############################################################################
  def run
    sync_out    = expand(options.sync_out)
    music_in    = expand(options.music_in)
    playlist_in = expand(options.playlist_in)

    versions_dir = File.join(sync_out, options.versions_dir)
    FileUtils.mkdir_p(versions_dir)

    current = File.join(versions_dir, checksum(playlist_in))
    return activate(current, sync_out) if Dir.exist?(current)

    begin
      music_out    = File.join(current,  options.music_out)
      playlist_out = File.join(current,  options.playlist_out)

      verbose("building output in #{current}")
      FileUtils.mkdir_p(music_out)
      FileUtils.mkdir_p(playlist_out)

      Dir.foreach(playlist_in) do |playlist|
        next if playlist.match(/^\./)
        verbose("reading playlist: #{playlist}")
        incoming = Playlist.read(File.join(playlist_in, playlist))

        File.open(File.join(playlist_out, playlist), 'w') do |corrected|
          incoming.items.each do |track|
            link_file(File.join(music_in, track), File.join(music_out, track))
            corrected.puts(File.join(options.music_prefix, track))
          end
        end
      end
    rescue
      FileUtils.rm_rf(current)
      raise
    end

    activate(current, sync_out)
  end

  ##############################################################################
  private

  ##############################################################################
  def verbose (*args)
    $stdout.puts(args.join(' ')) if options.verbose
  end

  ##############################################################################
  def expand (dir)
    File.expand_path(dir) % [options.device]
  end

  ##############################################################################
  def checksum (playlist_in)
    digest = Digest::SHA256.new

    Dir.foreach(playlist_in) do |file|
      next if file.match(/^\./)
      digest.update(File.read(File.join(playlist_in, file)))
    end

    digest.hexdigest
  end

  ##############################################################################
  def link_file (src, dst)
    # May be already linked from another playlist.
    return if File.exist?(dst)

    FileUtils.mkdir_p(File.dirname(dst))
    FileUtils.ln_s(src, dst)
  end

  ##############################################################################
  def activate (checksum, out)
    current = File.join(out, 'current')
    verbose("setting current link to #{File.basename(checksum)}")
    FileUtils.ln_s(checksum, current, :force => true)
  end
end

################################################################################
begin
  Sync.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
