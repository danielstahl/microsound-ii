package music

import music.Instruments.ARControlInstrumentBuilder._
import music.Instruments.LineControlInstrumentBuilder._
import music.Instruments._
import net.soundmining.MusicPlayer
import net.soundmining.Utils._
import Piece._
import net.soundmining.Melody._

/**
 * Third movement in microsound II
 *
 * Lower Pulse
 * Use theme from first movement
 * 1. Delta time. Make the duration a variant of that theme. Invert, retrograde or something.*
 * 2. Pulse speed
 * 3. The upper part. Transpose pitch up. Also use time.
 *
 */
object Movement3 {

  def playPulse(time: Float, note: PulseNote)(implicit player: MusicPlayer): Unit = {
    val dur = note.delta - (1f / note.pulseEndTime / 2)
    val outBus = note.soundBus

    val pulse = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(outBus)
      .dur(dur)
      .freqBus.control(line(note.delta, note.pulseStartTime, note.pulseEndTime))
      .widthBus.control(line(dur, 0.5f, 0.5f))
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    val highPass = highPassReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(outBus)
      .dur(dur)
      .freqBus.control(note.lower)
      .buildInstruments()

    val lowPass = lowPassReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(outBus)
      .dur(dur)
      .freqBus.control(note.upper)
      .buildInstruments()


    val volume = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .in(outBus)
      .out(0)
      .dur(dur)
      .ampBus.control(note.volume)
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(time), (pulse ++ highPass ++ lowPass ++ volume).toSeq: _*)

  }

  case class PulseNote(pulseStartTime: Float, pulseEndTime: Float, nrOfPulses: Float,
                       lowerControl: ControlInstrumentBuilder, upperControl: ControlInstrumentBuilder,
                       volumeControl: ControlInstrumentBuilder, soundBus: Int = 16) {

    lazy val delta: Float = {
      val startDur = (1f / pulseStartTime) * nrOfPulses
      val endDur = (1f / pulseEndTime) * nrOfPulses
      val avgDur = (startDur + endDur) / 2f
      avgDur
    }

    def lower: ControlInstrumentBuilder = {
      lowerControl.dur(delta)
      lowerControl
    }

    def upper: ControlInstrumentBuilder = {
      upperControl.dur(delta)
      upperControl
    }

    def volume: ControlInstrumentBuilder = {
      volumeControl.dur(delta)
      volumeControl
    }
  }


  def playPulseNotes(notes: Seq[PulseNote], startTime: Float)(implicit player: MusicPlayer): Unit = {
    var tempTime = startTime
    (0 until notes.length).foreach {
      i =>
        playPulse(tempTime, notes(i))
        tempTime += notes(i).delta
    }
  }


  case class PulseMelodyData(pulse: Seq[Int], nbrOfPulses: Seq[Int], lower: Seq[Int], upper: Seq[Int], volumeControl: Seq[ControlInstrumentBuilder], soundBus: Int = 16) {
    val startPulse = pulse
    val endPulse = shift(1, pulse)

    val lowerStart = lower
    val lowerEnd = shift(1, lowerStart)

    val upperStart = upper
    val upperEnd = shift(1, upper)

    def make: Seq[PulseNote] = {
      (0 until pulse.length).map {
        i =>
          PulseNote(
            underSpectrum(startPulse(i)), underSpectrum(endPulse(i)), nbrOfPulses(i),
            line(1f, overSpectrum(lowerStart(i)), overSpectrum(lowerEnd(i))),
            line(1f, overSpectrum(upperStart(i)), overSpectrum(upperEnd(i))),
            volumeControl(i), soundBus)
      }


    }
  }

  def thirdMovement(): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    val increaseVolume = line(1f, 0.1f, 3f)
    val decreaseVolume = line(1f, 3f, 0.1f)
    val volumes = Seq(increaseVolume, decreaseVolume, increaseVolume, decreaseVolume, increaseVolume, decreaseVolume)

    val lowPulseMelody = PulseMelodyData(
      pulse = Seq(13, 20, 13, 20, 13, 20),
      nbrOfPulses = Seq(21, 21, 8, 8, 13, 13),
      lower = Seq(3, 2, 3, 2, 3, 2),
      upper = Seq(4, 5, 4, 5, 4, 5),
      volumeControl = volumes
    )

    val lowPulseNotes = lowPulseMelody.make

    val highPulseMelody = PulseMelodyData(
      pulse = Seq(30, 36, 30, 36, 30, 36),
      nbrOfPulses = Seq(13, 13, 8, 8, 5, 5),
      lower = Seq(60, 63, 60, 63, 60, 63),
      upper = Seq(70, 64, 70, 64, 70, 64),
      volumeControl =  volumes,
      soundBus = 17
    )

    val highPulseNotes = highPulseMelody.make

    val middlePulseMelody = PulseMelodyData(
      pulse = Seq(21, 23, 21, 23),
      nbrOfPulses = Seq(21, 21, 13, 13),
      lower = Seq(30, 25, 30, 25),
      upper = Seq(40, 35, 40, 35),
      volumeControl = volumes,
      soundBus = 18
    )

    val middlePulseNotes =middlePulseMelody.make

    player.startPlay()

    setupNodes(player)

    playPulseNotes(lowPulseNotes, 0f)
    playPulseNotes(middlePulseNotes, (1f / underSpectrum(21)) * 3)
    playPulseNotes(highPulseNotes, (1f / underSpectrum(30)) * 8)


    Thread.sleep(5000)
  }

  def main(args: Array[String]): Unit = {
    thirdMovement()
  }
}
