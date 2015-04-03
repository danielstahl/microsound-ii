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


    val volume = monoReplaceVolumeInstrument
      .addAction(TAIL_ACTION)
      .in(outBus)
      .dur(dur)
      .ampBus.control(note.pulseVolume)
      .buildInstruments()

    val whiteNoise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .out(outBus + 3)
      .ampBus.control(line(dur, 0.4f, 0.4f))
      .buildInstruments()

      val filterLower = filterReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(outBus + 3)
      .ampBus.control(note.lowerNoiseVolume)
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(note.lower)
      .buildInstruments()

    val filterUpper = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(outBus + 3)
      .out(outBus)
      .ampBus.control(note.upperNoiseVolume)
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(note.upper)
      .buildInstruments()

    val lowerNote = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(outBus)
      .dur(dur)
      .freqBus.control(note.lower)
      .widthBus.control(line(dur, 0.5f, 0.5f))
      .ampBus.control(note.lowerPulseVolume)
      .buildInstruments()

    val upperNote = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(outBus)
      .dur(dur)
      .freqBus.control(note.upper)
      .widthBus.control(line(dur, 0.5f, 0.5f))
      .ampBus.control(note.upperPulseVolume)
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .in(outBus)
      .out(0)
      .dur(dur)
      .panBus.control(note.pan)
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(time), (pulse ++ highPass ++ lowPass ++ volume ++ whiteNoise ++ filterLower ++ filterUpper ++ lowerNote ++  upperNote ++ pan).toSeq: _*)

  }

  case class PulseNote(pulseStartTime: Float, pulseEndTime: Float, nrOfPulses: Float,
                       lowerControl: ControlInstrumentBuilder, upperControl: ControlInstrumentBuilder,
                       pulseVolumeControl: ControlInstrumentBuilder,
                       lowerPulseVolumeControl: ControlInstrumentBuilder,
                       upperPulseVolumeControl: ControlInstrumentBuilder,
                       lowerNoiseVolumeControl: ControlInstrumentBuilder,
                       upperNoiseVolumeControl: ControlInstrumentBuilder,
                       panControl: ControlInstrumentBuilder, soundBus: Int = 16) {

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

    def pulseVolume: ControlInstrumentBuilder = {
      pulseVolumeControl.dur(delta)
      pulseVolumeControl
    }

    def lowerPulseVolume: ControlInstrumentBuilder = {
      lowerPulseVolumeControl.dur(delta)
      lowerPulseVolumeControl
    }

    def upperPulseVolume: ControlInstrumentBuilder = {
      upperPulseVolumeControl.dur(delta)
      upperPulseVolumeControl
    }

    def lowerNoiseVolume: ControlInstrumentBuilder = {
      lowerNoiseVolumeControl.dur(delta)
      lowerNoiseVolumeControl
    }

    def upperNoiseVolume: ControlInstrumentBuilder = {
      upperNoiseVolumeControl.dur(delta)
      upperNoiseVolumeControl
    }

    def pan: ControlInstrumentBuilder = {
      panControl.dur(delta)
      panControl
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


  case class PulseMelodyData(pulse: Seq[Int], nbrOfPulses: Seq[Int], lower: Seq[Int], upper: Seq[Int],
                             pulseVolumeControl: Seq[ControlInstrumentBuilder],
                             lowerPulseVolumeControl: Seq[ControlInstrumentBuilder], upperPulseVolumeControl: Seq[ControlInstrumentBuilder],
                             lowerNoiseVolumeControl: Seq[ControlInstrumentBuilder], upperNoiseVolumeControl: Seq[ControlInstrumentBuilder],
                             panControl: Seq[ControlInstrumentBuilder], soundBus: Int = 16) {
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
            pulseVolumeControl(i), lowerPulseVolumeControl(i), upperPulseVolumeControl(i),
            lowerNoiseVolumeControl(i), upperNoiseVolumeControl(i), panControl(i), soundBus)
      }

    }
  }

  def thirdMovement(): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    def attackVolume(peak: Float) = ar(1, 0.3f, (0.0f, peak, 0.0f))
    def decayVolume(peak: Float) = ar(1, 0.7f, (0.0f, peak, 0.0f))
    def middleVolume(peak: Float) = ar(2, 0.5f, (0.0f, peak, 0.0f))

    val pulseVolumes = Seq(middleVolume(3f), decayVolume(3f), attackVolume(3f), middleVolume(3f), decayVolume(3f), attackVolume(3f))
    val lowerPulseVolumes = Seq(attackVolume(0.01f), middleVolume(0.01f), decayVolume(0.01f), attackVolume(0.01f), middleVolume(0.01f), decayVolume(0.01f))
    val upperPulseVolumes = Seq(decayVolume(0.01f), attackVolume(0.01f), middleVolume(0.01f), decayVolume(0.01f), attackVolume(0.01f), middleVolume(0.01f))
    val lowerNoiseVolumes = Seq(attackVolume(0.4f), middleVolume(0.4f), decayVolume(0.4f), attackVolume(0.4f), middleVolume(0.4f), decayVolume(0.4f))
    val upperNoiseVolumes = Seq(decayVolume(0.4f), attackVolume(0.4f), middleVolume(0.4f), decayVolume(0.4f), attackVolume(0.4f), middleVolume(0.4f))

    val mostLefToSlightLeft = line(1f, -1f, -0.5f)

    val slightLeftToSlightRight = line(1f, -0.5f, 0.5f)

    val slightRightToMostRight = line(1f, 0.5f, 1f)

    val lowPulseMelody = PulseMelodyData(
      pulse = Seq(13, 20, 13, 20, 13, 20),
      nbrOfPulses = Seq(21, 21, 8, 8, 13, 13),
      lower = Seq(3, 2, 3, 2, 3, 2),
      upper = Seq(4, 5, 4, 5, 4, 5),
      pulseVolumeControl = pulseVolumes,
      lowerPulseVolumeControl = lowerPulseVolumes,
      upperPulseVolumeControl = upperPulseVolumes,
      lowerNoiseVolumeControl = lowerNoiseVolumes,
      upperNoiseVolumeControl = upperNoiseVolumes,
      panControl = Seq(
        mostLefToSlightLeft, mostLefToSlightLeft.reverse,
        mostLefToSlightLeft, mostLefToSlightLeft.reverse,
        mostLefToSlightLeft, mostLefToSlightLeft.reverse)
    )

    val lowPulseNotes = lowPulseMelody.make

    val highPulseMelody = PulseMelodyData(
      pulse = Seq(30, 36, 30, 36/*, 30, 36*/),
      nbrOfPulses = Seq(13, 13, 8, 8, 5, 5),
      lower = Seq(60, 63, 60, 63, 60, 63),
      upper = Seq(70, 64, 70, 64, 70, 64),
      pulseVolumeControl = pulseVolumes,
      lowerPulseVolumeControl = lowerPulseVolumes,
      upperPulseVolumeControl = upperPulseVolumes,
      lowerNoiseVolumeControl = lowerNoiseVolumes,
      upperNoiseVolumeControl = upperNoiseVolumes,
      panControl = Seq(
        slightRightToMostRight.reverse, slightRightToMostRight,
        slightRightToMostRight.reverse, slightRightToMostRight,
        slightRightToMostRight.reverse, slightRightToMostRight),
      soundBus = 17
    )

    val highPulseNotes = highPulseMelody.make

    val middlePulseMelody = PulseMelodyData(
      pulse = Seq(21, 23, 21, 23),
      nbrOfPulses = Seq(21, 21, 13, 13),
      lower = Seq(30, 25, 30, 25),
      upper = Seq(40, 35, 40, 35),
      pulseVolumeControl = pulseVolumes,
      lowerPulseVolumeControl = lowerPulseVolumes,
      upperPulseVolumeControl = upperPulseVolumes,
      lowerNoiseVolumeControl = lowerNoiseVolumes,
      upperNoiseVolumeControl = upperNoiseVolumes,
      panControl = Seq(
        slightLeftToSlightRight, slightLeftToSlightRight.reverse,
        slightLeftToSlightRight, slightLeftToSlightRight.reverse,
        slightLeftToSlightRight, slightLeftToSlightRight.reverse
      ),
      soundBus = 18
    )

    val middlePulseNotes = middlePulseMelody.make

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
