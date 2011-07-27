class Alarm::ITunes
  
  ##############################################################################
  MAX_VOL = 70
  
  ##############################################################################
  def initialize
    @app = Appscript.app('iTunes.app')
    @app.activate
  end
  
  ##############################################################################
  def volume= (n)
    @app.sound_volume.set(n)
  end
  
  ##############################################################################
  def start_playlist (name)
    playlist = @app.playlists[name].get
    @app.play(playlist)
  end
  
  ##############################################################################
  def fade_in
    (1..MAX_VOL).each do |level|
      self.volume = level
      sleep(3)
    end
  end
  
  ##############################################################################
  def fade_in_playlist (name)
    self.volume = 1
    start_playlist(name)
    fade_in
  end
end
