class Alarm::ITunes
  
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
  def playing?
    @app.player_state.get == :playing
  end
  
  ##############################################################################
  def fade_in (options={})
    options = {
      :speed => :slow,
    }.merge(options)

    delay = options[:speed] == :slow ? 3 : 0.1

    (1..Alarm::MAX_VOL).each do |level|
      self.volume = level
      sleep(delay)
    end
  end
  
  ##############################################################################
  def fade_in_playlist (name, options={})
    self.volume = 1
    start_playlist(name)
    fade_in(options)
  end

  ##############################################################################
  def fade_out
    (0..@app.sound_volume.get).to_a.reverse.each do |level|
      self.volume = level
      sleep(0.1)
    end
  end
  
  ##############################################################################
  def fade_out_and_stop
    fade_out
    @app.stop
  end
end
