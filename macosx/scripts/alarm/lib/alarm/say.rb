class Alarm::Say
  
  ##############################################################################
  def initialize (airfoil)
    @airfoil = airfoil
  end

  ##############################################################################
  # Can't use Tempfile here because QuickTime refuses to open such files.
  def speak_file (name, options={})
    options = {
      :voice => Alarm::DEFAULT_VOICE,
    }.merge(options)

    FileUtils.mkdir_p(Alarm::TEMP_DIR)
    audio_file = File.join(Alarm::TEMP_DIR, 'alarm_speak_file.aiff')
    
    # Wait until Lion update
    #system('say', '-v', options[:voice], '-f', name, '-o', audio_file)
    system('say', '-f', name, '-o', audio_file)
      

    qt = Appscript.app('QuickTime Player.app')
    qt_running = qt.is_running?
    
    if !qt_running
      qt.launch
      sleep(3)
    end

    @airfoil.get_audio_from('QuickTime Player')

    qt.open(audio_file)
    sleep(1)

    doc = qt.documents[File.basename(audio_file)]
    doc.audio_volume.set(1.0)
    doc.looping.set(false)
    doc.muted.set(false)
    doc.play
    sleep(doc.duration.get + 10) # compensate for playback delay
    doc.close
  ensure
    qt.quit if qt and !qt_running
    File.unlink(audio_file) if audio_file and File.exist?(audio_file)
  end
  
  ##############################################################################
  def speak_string (str, options={})
    Tempfile.open('alarm_say_txt') do |file|
      file.puts(str)
      file.close
      speak_file(file.path, options)
    end
  end

  ##############################################################################
  def speak_weather (zipcode, options={})
    FileUtils.mkdir_p(Alarm::TEMP_DIR)
    text_file = File.join(Alarm::TEMP_DIR, 'weather.txt')
    system("#{Alarm::WEATHER_SCRIPT} #{zipcode.to_i} > #{text_file}")
    speak_file(text_file, options)
  ensure
    File.unlink(text_file) if text_file and File.exist?(text_file)
  end
end
