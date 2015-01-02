#!ruby

################################################################################
require('fileutils')
require('open3')
require('optparse')
require('ostruct')
require('tmpdir')
require('yaml')

################################################################################
class Cfg

  ##############################################################################
  # Device entries.
  Device = Struct.new(:name, :music_dir, :music_prefix, :playlist_dir, :playlists)

  ##############################################################################
  attr_accessor(:directory, :playlist_dir, :devices)

  ##############################################################################
  def self.open (file)
    raw = YAML.load_file(file)

    raise("no `sync' entry in #{file}") unless raw.has_key?('sync')
    sync = raw['sync']

    devices = sync['devices'].reduce({}) do |h, settings|
      h[settings['name']] =
        Device.new(settings['name'],
                   File.expand_path(settings['music_dir']),
                   settings['music_prefix'],
                   File.expand_path(settings['playlist_dir']),
                   Array(settings['playlists']))
      h
    end

    cfg = new
    cfg.directory = File.expand_path(raw['directory'])
    cfg.playlist_dir = File.expand_path(sync['playlist_dir'])
    cfg.devices = devices
    cfg
  end
end

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
    :config  => `beet config -p`.chomp,
    :verbose => false,
    :device  => nil,
  }

  ##############################################################################
  attr_reader(:options, :config, :device)

  ##############################################################################
  def initialize
    @options = OpenStruct.new(DEFAULT_OPTIONS)

    OptionParser.new do |p|
      p.on('-h', '--help', 'This message') {puts(p); exit}
      p.on('--[no-]verbose', 'Turn on/off verbosity') {|x| options.verbose = x}
      p.on('-c', '--config=FILE', 'Beets config') {|x| options.config = x}
      p.on('-d', '--device=NAME', 'Device to sync') {|x| options.device = x}
    end.parse!(ARGV)

    @config = Cfg.open(options.config)

    if options.device.nil?
      raise("you must give --device")
    elsif !config.devices.has_key?(options.device)
      raise("no such device in config: #{options.device}")
    end

    @device = config.devices[options.device]
  end

  ##############################################################################
  def run
    Dir.mktmpdir('beet-sync') do |tmpdir|
      verbose("temporary playlist directory: #{tmpdir}")
      tmp_music_dir    = File.join(tmpdir,    'music')
      tmp_playlist_dir = File.join(tmpdir, 'playlists')

      FileUtils.mkdir_p(tmp_music_dir)
      FileUtils.mkdir_p(tmp_playlist_dir)
      FileUtils.mkdir_p(device.music_dir)
      FileUtils.mkdir_p(device.playlist_dir)

      verbose("creating tmp music and playlist directories")

      device.playlists.each do |playlist|
        verbose("reading playlist: #{playlist}")
        incoming = Playlist.read(File.join(config.playlist_dir, playlist))

        File.open(File.join(tmpdir, playlist), 'w') do |corrected|
          incoming.items.each do |file_name|
            link_file(file_name, tmp_music_dir)
            corrected.puts(File.join(device.music_prefix, file_name))
          end
        end
      end

      verbose("syncing music")
      system(*rsync_music_command(tmp_music_dir)) ||
        raise("music sync failed")

      verbose("syncing playlists")
      system(*rsync_playlists_command(tmp_playlist_dir)) ||
        raise("playlist sync failed")
    end
  end

  ##############################################################################
  private

  ##############################################################################
  def rsync_command
    cmd = [
      'rsync',
      '--times',
      '--copy-links',
      '--delete-before',
      '--recursive',
      '--prune-empty-dirs',
    ]

    if options.verbose
      cmd << '--verbose'
    else
      cmd << '-q'
    end

    cmd
  end

  ##############################################################################
  def rsync_music_command (tmpdir)
    cmd = rsync_command
    cmd << tmpdir + '/'
    cmd << device.music_dir + '/'
    cmd
  end

  ##############################################################################
  def rsync_playlists_command (tmpdir)
    cmd = rsync_command
    cmd << tmpdir + "/"
    cmd << device.playlist_dir + "/"
    cmd
  end

  ##############################################################################
  def link_file (src, dst)
    full_src = File.join(config.directory, src)
    full_dst = File.join(dst, src)

    FileUtils.mkdir_p(File.dirname(full_dst))
    FileUtils.ln_s(full_src, full_dst)
  end

  ##############################################################################
  def verbose (*args)
    $stdout.puts(args.join(' ')) if options.verbose
  end
end

################################################################################
begin
  Sync.new.run
rescue RuntimeError => e
  $stderr.puts(File.basename($0) + ": ERROR: #{e}")
  exit(1)
end
