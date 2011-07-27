class Alarm::Airfoil
  
  ##############################################################################
  attr_accessor(:default_speaker)
  
  ##############################################################################
  def initialize
    @app = Appscript.app('Airfoil.app')
    @app.activate
    @app.linked_volume.set(false)

    @default_speaker = 'Mobile Audio'
    @verbose = true
    
    disconnect_all_speakers
  end
  
  ##############################################################################
  def get_audio_from (source="iTunes")
    Appscript.app("#{source}.app").quit
    sleep(5)
    src = @app.application_sources[source].get
    @app.current_audio_source.set(src)
    connect_to_speaker(@default_speaker)
  end

  ##############################################################################
  def disconnect_all_speakers
    speakers = @app.speakers.get
    @app.disconnect_from(speakers)
  end
  
  ##############################################################################
  def connect_to_speaker (name)
    speaker = @app.speakers[name].get
    speaker.volume.set(1.0)
    @app.connect_to(speaker)
    sleep(10)
    
    if @verbose
      @app.speakers.get.each do |speaker|
        $stderr.puts("Speaker: #{speaker.name.get}\t\tConnected: #{speaker.connected.get}")
      end
    end
  end
  
  ##############################################################################
  def speak_weather (zipcode)
    FileUtils.mkdir_p(Alarm::TEMP_DIR)
    text_file = File.join(Alarm::TEMP_DIR, 'weather.txt')
    system("#{Alarm::WEATHER_SCRIPT} #{zipcode.to_i} > #{text_file}")
    speak_file(text_file)
  ensure
    File.unlink(text_file) if text_file and File.exist?(text_file)
  end
  
  ##############################################################################
  # Can't use Tempfile here because QuickTime refuses to open such files.
  def speak_file (name)
    FileUtils.mkdir_p(Alarm::TEMP_DIR)
    audio_file = File.join(Alarm::TEMP_DIR, 'alarm_speak_file.aiff')
    system(*%W(say -f #{name} -o #{audio_file}))
      
    get_audio_from('QuickTime Player')
    qt = Appscript.app('QuickTime Player.app')

    qt.activate
    qt.open(audio_file)
    sleep(1)

    doc = qt.documents[File.basename(audio_file)]
    doc.audio_volume.set(1.0)
    doc.looping.set(false)
    doc.muted.set(false)
    doc.play
    sleep(doc.duration.get + 10) # compensate for playback delay
    doc.close
    qt.quit
  ensure
    File.unlink(audio_file) if audio_file and File.exist?(audio_file)
  end
  
  ##############################################################################
  def speak_string (str)
    Tempfile.open('alarm_say_txt') do |file|
      file.puts(str)
      file.close
      speak_file(file.path)
    end
  end
  
  ##############################################################################
  def quit
    @app.quit
  end
end
