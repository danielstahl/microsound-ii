package music

import music.Instruments.ARControlInstrumentBuilder._
import music.Instruments.ASRControlInstrumentBuilder._
import music.Instruments.LineControlInstrumentBuilder._
import music.Instruments.SineControlReplaceInstrumentBuilder._
import music.Instruments._
import music.Piece._

/**
 * The idea is to write a short piece that is built
 * on point musc. Much like some of Weberns pieces or
 * Kontra punkte by Stockhausen.
 *
 * Each note should be clearly separated. Pitch, timbre and time.
 *
 * Different themes
 * Pitch
 * Duration
 * Length
 * Attack curve
 * Timbre
 */
object Movement1 {

  import Utils._
  import Melody._

  def playNote(start: Float, dur: Float, freq: Float)(implicit player: MusicPlayer): Unit = {
    val pulse = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(16)
      .dur(dur)
      .freqBus.control(line(dur, freq, freq))
      .widthBus.control(line(dur, 0.5f, 0.5f))
      .ampBus.control(line(dur, 0.1f, 0.1f))
      .buildInstruments()


    player.sendNew(absoluteTimeToMillis(start), pulse.toSeq: _*)
  }

  def playMelody(melody: Seq[Float])(implicit player: MusicPlayer): Unit = {
    (0 until melody.length).map {
      i => playNote(i, 0.5f, melody(i))
    }
  }

  def playMelody(melody: Seq[Float], durMelody: Seq[Float])(implicit player: MusicPlayer): Unit = {
    var tmpTime = 0f
    (0 until melody.length).map {
      i =>
        playNote(tmpTime, 0.5f, melody(i))
        tmpTime = tmpTime + durMelody(i)
    }
  }

  def note1(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 5f
    val bus1 = note.soundBus
    val bus2 = note.soundBus + 1
    val bus3 = note.soundBus + 2

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus1)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val filter1 = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(bus2)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.01f, 0.0f)))
      .bwBus.control(line(dur, 0.000000000000000001f, 0.000000000000000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val pan1 = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(0)
      .panBus.control(line(dur, 0f, -1))
      .buildInstruments()

    val effect1 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()


    val filter2 = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(bus3)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.01f, 0.0f)))
      .bwBus.control(line(dur, 0.000000000001f, 0.000000000001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pan2 = panInstrument
      .addAction(TAIL_ACTION)
      .dur(note.deltaTime)
      .in(bus3)
      .out(0)
      .panBus.control(line(dur, 0f, -0.5f))
      .buildInstruments()

    val effect2 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus3)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (noise ++ filter1 ++ pan1 ++ effect1 ++ filter2 ++ pan2 ++ effect2).toSeq:_*)
  }

  def note2(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.1f
    val dur2 = 1.5f

    val bus1 = note.soundBus
    val bus2 = note.soundBus

    val pulse1 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus1)
      .dur(dur)
      .freqBus.control(line(dur, 0.1f, 0.1f))
      .widthBus.control(line(dur, 0.1f, 0.9f))
      .ampBus.control(asr(dur, (0.0f, 0.05f, 0.02f, 0.0f), (0.01f, 8f, 3f)))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.1f, 0.0f)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val pulse2 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus2)
      .dur(dur2)
      .freqBus.control(line(dur2, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .widthBus.control(line(dur2, 0.1f, 0.9f))
      .ampBus.control(asr(dur2, (0.0f, 0.02f, 0.01f, 0.0f), (0.1f, 8f, 3f)))
      .buildInstruments()

    val pulse1Pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(0.1f)
      .in(bus1)
      .out(0)
      .panBus.control(line(dur, 0.5f, 0.8f))
      .buildInstruments()

    val effect1 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    val pulse2Pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur2)
      .in(bus2)
      .out(0)
      .panBus.control(line(dur2, 0.6f, 0f))
      .buildInstruments()

    val effect2 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (pulse1 ++ filter1 ++ pulse2 ++ pulse1Pan ++ effect1 ++ pulse2Pan ++ effect2).toSeq:_*)
  }

  def note3(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.1f
    val bus = note.soundBus

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.6f, 0.0f)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val filter2 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.4f, 0.0f)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(note.deltaTime)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, 0.5f, -0.5f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (noise ++ filter1 ++ filter2 ++ pan ++ effect).toSeq:_*)
  }

  def note4(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.8f
    val bus = note.soundBus
    val pulse1 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .widthBus.control(line(dur, 0.1f, 0.5f))
      .ampBus.control(ar(dur, 0.9f, (0.0f, 0.05f, 0.0f)))
      .buildInstruments()

    val pulse2 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .widthBus.control(line(dur, 0.5f, 0.1f))
      .ampBus.control(ar(dur, 0.1f, (0.0f, 0.05f, 0.0f)))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, -0.5f, 0.5f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (pulse1 ++ pulse2 ++ pan ++ effect).toSeq:_*)
  }


  def note5(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 1f
    val bus = note.soundBus
    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.1f, 0.0f)))
      .bwBus.control(line(dur, 0.001f, 0.001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val filter2 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.2f, 0.0f)))
      .bwBus.control(line(dur, 0.00001f, 0.00001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(note.deltaTime)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, 0f, 0.9f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (noise ++ filter1 ++ filter2 ++ pan ++ effect).toSeq:_*)
  }

  def note6(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.1f
    val dur2 = 1.5f
    val bus1 = note.soundBus
    val bus2 = note.soundBus + 1

    val pulse1 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus1)
      .dur(dur)
      .freqBus.control(line(dur, 0.1f, 0.1f))
      .widthBus.control(line(dur, 0.1f, 0.9f))
      .ampBus.control(asr(dur, (0.0f, 0.05f, 0.02f, 0.0f), (0.01f, 8f, 3f)))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.3f, 0.0f)))
      .bwBus.control(line(dur, 0.000000001f, 0.000000001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pulse2 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus2)
      .dur(dur2)
      .freqBus.control(line(dur2, note.melodyNote, note.melodyNote))
      .widthBus.control(line(dur2, 0.1f, 0.9f))
      .ampBus.control(asr(dur2, (0.0f, 0.03f, 0.01f, 0.0f), (0.1f, 8f, 3f)))
      .buildInstruments()

    val pulse1Pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(0.1f)
      .in(bus1)
      .out(0)
      .panBus.control(line(dur, -0.5f, -0.8f))
      .buildInstruments()

    val effect1 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    val pulse2Pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur2)
      .in(bus2)
      .out(0)
      .panBus.control(line(dur2, -0.6f, 0f))
      .buildInstruments()

    val effect2 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (pulse1 ++ filter1 ++ pulse2 ++ pulse1Pan ++ effect1 ++ pulse2Pan ++ effect2).toSeq:_*)
  }

  def note7(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = note.deltaTime
    val bus1 = note.soundBus
    val bus2 = note.soundBus + 1
    val bus3 = note.soundBus + 2

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus1)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val filter1 = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(bus2)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.01f, 0.0f)))
      .bwBus.control(line(dur, 0.000000000001f, 0.000000000001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pan1 = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(0)
      .panBus.control(line(dur, 0f, -1))
      .buildInstruments()

    val effect1 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    val filter2 = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(bus3)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.01f, 0.0f)))
      .bwBus.control(line(dur, 0.000000000000000001f, 0.000000000000000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val pan2 = panInstrument
      .addAction(TAIL_ACTION)
      .dur(note.deltaTime)
      .in(bus3)
      .out(0)
      .panBus.control(line(dur, 0f, -0.5f))
      .buildInstruments()

    val effect2 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus3)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (noise ++ filter1 ++ pan1 ++ effect1 ++ filter2 ++ pan2 ++ effect2).toSeq:_*)
  }

  def note8(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.2f
    val bus = note.soundBus

    val pulse1 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .widthBus.control(line(dur, 0.5f, 0.1f))
      .ampBus.control(ar(dur, 0.1f, (0.0f, 0.05f, 0.0f), (EXPONENTIAL, EXPONENTIAL)))
      .buildInstruments()

    val pulse2 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .widthBus.control(line(dur, 0.1f, 0.5f))
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.05f, 0.0f), (SINE, SINE)))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, -0.5f, 0.5f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (pulse1 ++ pulse2 ++ pan ++ effect).toSeq:_*)

  }

  def note9(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.1f
    val bus = note.soundBus

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.1f, (0.0f, 0.5f, 0.0f), (EXPONENTIAL, EXPONENTIAL)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val filter2 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.3f, 0.0f), (SINE, SINE)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(note.deltaTime)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, 1f, 0f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (noise ++ filter1 ++ filter2 ++ pan ++ effect).toSeq:_*)
  }

  def note10(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.1f
    val bus = note.soundBus

    val pulse1 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, 0.1f, 0.1f))
      .widthBus.control(line(dur, 0.1f, 0.9f))
      .ampBus.control(asr(dur, (0.0f, 0.05f, 0.02f, 0.0f), (0.01f, 8f, 3f)))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.2f, 0.0f)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()


    val filter2 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.5f, 0.0f)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(0.1f)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, -0.5f, -0.5f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (pulse1 ++ filter1 ++ filter2 ++ pulsePan ++ effect).toSeq:_*)
  }

  def note11(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = note.deltaTime
    val bus = note.soundBus
    val pulse1 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .widthBus.control(line(dur, 0.5f, 0.9f))
      .ampBus.control(ar(dur, 0.1f, (0.0f, 0.01f, 0.0f)))
      .buildInstruments()

    val pulse2 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .widthBus.control(line(dur, 0.9f, 0.5f))
      .ampBus.control(ar(dur, 0.9f, (0.0f, 0.05f, 0.0f)))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, -0.5f, -1f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (pulse1 ++ pulse2 ++ pan ++ effect).toSeq:_*)
  }

  def note12(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.8f
    val bus = note.soundBus
    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.1f, 0.0f)))
      .bwBus.control(line(dur, 0.001f, 0.001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val filter2 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.2f, 0.0f)))
      .bwBus.control(line(dur, 0.00001f, 0.00001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(note.deltaTime)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, 0.5f, 1f))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (noise ++ filter1 ++ filter2 ++ pan ++ effect).toSeq:_*)
  }

  def note13(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = 0.1f
    val dur2 = note.deltaTime

    val bus1 = note.soundBus
    val bus2 = note.soundBus

    val pulse1 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus1)
      .dur(dur)
      .freqBus.control(line(dur, 0.1f, 0.1f))
      .widthBus.control(line(dur, 0.1f, 0.9f))
      .ampBus.control(asr(dur, (0.0f, 0.05f, 0.02f, 0.0f), (0.01f, 8f, 3f)))
      .buildInstruments()

    val filter1 = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.1f, 0.0f)))
      .bwBus.control(line(dur, 0.0000001f, 0.0000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val pulse2 = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(bus2)
      .dur(dur2)
      .freqBus.control(line(dur2, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .widthBus.control(line(dur2, 0.1f, 0.9f))
      .ampBus.control(asr(dur2, (0.0f, 0.02f, 0.01f, 0.0f), (0.1f, 8f, 3f)))
      .buildInstruments()

    val pulse1Pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(0.1f)
      .in(bus1)
      .out(0)
      .panBus.control(line(dur, 0.5f, 0.8f))
      .buildInstruments()

    val effect1 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    val pulse2Pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur2)
      .in(bus2)
      .out(0)
      .panBus.control(line(dur2, 0.6f, 0f))
      .buildInstruments()

    val effect2 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(note.startTime), (pulse1 ++ filter1 ++ pulse2 ++ pulse1Pan ++ effect1 ++ pulse2Pan ++ effect2).toSeq:_*)
  }

  def note14(note: Note)(implicit player: MusicPlayer): Unit = {
    val dur = note.deltaTime
    val bus1 = note.soundBus
    val bus2 = note.soundBus + 1
    val bus3 = note.soundBus + 2

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus1)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val filter1 = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(bus2)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.01f, 0.0f)))
      .bwBus.control(line(dur, 0.000000000000000001f, 0.000000000000000001f))
      .freqBus.control(line(dur, note.melodyNote, note.melodyNote))
      .buildInstruments()

    val pan1 = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(0)
      .panBus.control(line(dur, 0f, -1))
      .buildInstruments()

    val effect1 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus2)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    val filter2 = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus1)
      .out(bus3)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.01f, 0.0f)))
      .bwBus.control(line(dur, 0.000000000001f, 0.000000000001f))
      .freqBus.control(line(dur, note.retrogradeMelodyNote, note.retrogradeMelodyNote))
      .buildInstruments()

    val pan2 = panInstrument
      .addAction(TAIL_ACTION)
      .dur(note.deltaTime)
      .in(bus3)
      .out(0)
      .panBus.control(line(dur, 0f, -0.5f))
      .buildInstruments()

    val effect2 = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus3)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()
    player.sendNew(absoluteTimeToMillis(note.startTime), (noise ++ filter1 ++ pan1 ++ effect1 ++ filter2 ++ pan2 ++ effect2).toSeq:_*)
  }

  def effect1(implicit player: MusicPlayer): Unit = {
    val bus = 22
    val dur = 50

    val volume = monoVolumeInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(23)
      .ampBus.control(line(dur, 0.1f, 0.1f))
      .buildInstruments()

    val delay = monoDelayReplaceInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(23)
      .delayBus.control(ar(dur, 0.3f, (0.3f, 0.55f, 0.2f), nodeId = EFFECT), sine(dur, underSpectrum(48), underSpectrum(48), 0.001f, 0.001f, nodeId = EFFECT))
      .maxDelay(0.5f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(23)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.9f, nodeId = EFFECT), sine(dur, 0.6f, 0.5f, 0.05f, 0.06f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.07f, 0.09f, nodeId = EFFECT), sine(dur, 0.7f, 0.3f, 0.01f, 0.008f, nodeId = EFFECT))
      .maxDelay(0.1f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(23)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.3f, 1.4f, nodeId = EFFECT), sine(dur, 0.09f, 0.1f, 0.05f, 0.09f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.01f, 0.009f, nodeId = EFFECT))
      .maxDelay(0.01f)
      .buildInstruments()

    val pan = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(23)
      .out(0)
      .panBus.control(line(dur, 1f, -1f, EFFECT))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(0), (volume ++ delay ++ combFilter ++ allpassFilter ++ pan).toSeq:_*)
  }

  def effect2(implicit player: MusicPlayer): Unit = {
    val bus = 24
    val dur = 50

    val volume = monoVolumeInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(25)
      .ampBus.control(line(dur, 0.05f, 0.05f))
      .buildInstruments()

    val delay = monoDelayReplaceInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(25)
      .delayBus.control(ar(dur, 0.3f, (0.01f, 0.1f, 0.02f), nodeId = EFFECT), sine(dur, underSpectrum(48), underSpectrum(48), 0.001f, 0.001f, nodeId = EFFECT))
      .maxDelay(0.2f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(25)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.2f, 1.3f, nodeId = EFFECT), sine(dur, underSpectrum(40), underSpectrum(48), 0.05f, 0.06f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.1f, 0.2f, nodeId = EFFECT), sine(dur, 0.7f, 0.3f, 0.01f, 0.008f, nodeId = EFFECT))
      .maxDelay(0.3f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(25)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.3f, 1.4f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.15f, 0.2f, nodeId = EFFECT), sine(dur, 0.09f, 0.1f, 0.05f, 0.09f, nodeId = EFFECT))
      .maxDelay(0.2f)
      .buildInstruments()

    val pan = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(25)
      .out(0)
      .panBus.control(line(dur, -1f, 1f, EFFECT))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(0), (volume ++ delay ++ combFilter ++ allpassFilter ++ pan).toSeq:_*)
  }

  def absolute(start: Float, relative: Seq[Float]): Seq[Float] = {
    var tmp = start
    relative.map {
      time =>
        val result = tmp
        tmp = tmp + time
        result
    }
  }

  case class Note(melodyNote: Float, retrogradeMelodyNote: Float, startTime: Float, deltaTime: Float, soundBus: Int, effectBus: Int)

  def firstMovement(): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    val melody =
      Seq(3, 2, 20, 21, 7, 14, 24, 15, 8, 9, 6, 11, 23, 18)

    val retrogradeMelody = retrograde(inverse(transpose(70, melody)))
    val concreteMelody = concrete(melody, overSpectrum)
    val concreteRetrogradeMelody = concrete(retrogradeMelody, overSpectrum)
    val concreteRhythm = concrete(melody, underSpectrum)
    val absoluteTime = absolute(0, concreteRhythm)

    val channels = Seq(16, 19, 16, 19, 16, 19, 16, 19, 16, 19, 16, 19, 16, 19)

    val effectBus = Seq(22, 24, 24, 22, 22, 24, 22, 24, 24, 24, 22, 22, 24, 24)

    val notes: Seq[Note => Unit] = Seq(note1, note2, note3, note4, note5, note6, note7, note8, note9, note10, note11, note12, note13, note14)

    effect1
    effect2

    (0 until notes.length).foreach {
      i =>
        notes(i)(Note(concreteMelody(i), concreteRetrogradeMelody(i), absoluteTime(i), concreteRhythm(i), channels(i), effectBus(i)))
    }

    Thread.sleep(1000)
  }

  def main(args: Array[String]): Unit = {
    firstMovement()
  }
}
