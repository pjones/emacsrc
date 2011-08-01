class Alarm::Airfoil
  
  ##############################################################################
  attr_accessor(:default_speaker)
  attr_accessor(:controlling)
  
  ##############################################################################
  def initialize
    @app = Appscript.app('Airfoil.app')
    
    if !@app.is_running?
      @app.activate
      sleep(5)
    end

    @app.linked_volume.set(false)
    @default_speaker = 'Mobile Audio'
  end
  
  ##############################################################################
  def get_audio_from (source="iTunes")
    disconnect_all_speakers
    src = @app.application_sources[source].get
    @app.current_audio_source.set(src)
    connect_to_speaker(@default_speaker)
  end

  ##############################################################################
  def disconnect_all_speakers
    @app.speakers.get.each do |speaker|
      @app.disconnect_from(speaker)
    end
  end
  
  ##############################################################################
  def connect_to_speaker (name)
    speaker = @app.speakers[name].get
    speaker.volume.set(1.0)
    @app.connect_to(speaker)
    sleep(10)
  end
  
  ##############################################################################
  def quit
    @app.quit
  end
end
