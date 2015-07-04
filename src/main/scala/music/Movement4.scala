package music

import music.Instruments.ARControlInstrumentBuilder._
import music.Instruments.LineControlInstrumentBuilder._
import music.Instruments.SineControlReplaceInstrumentBuilder._
import music.Instruments._
import net.soundmining.Instrument.{ROOM_EFFECT, EFFECT, TAIL_ACTION}
import net.soundmining.{BusGenerator, ControlInstrumentBuilder, MusicPlayer}
import net.soundmining.Utils._
import Piece._
import net.soundmining.Melody._
import net.soundmining.Instrument._

/**
 * Fourth movement in microsound II
 *
 * Lower Pulse
 * Use theme from first movement
 * 1. Delta time. Make the duration a variant of that theme. Invert, retrograde or something.*
 * 2. Pulse speed
 * 3. The upper part. Transpose pitch up. Also use time.
 *
 * The effect
 * Try phasing effect
 * Allpass with Sine on delaytime
 * See http://community.dur.ac.uk/nick.collins/teaching/supercollider/sctutorial/6.4%20Effects%201.html
 *
 *
 */
object Movement4 {

  def playNoise(time: Float, duration: Float, noiseFilters: Seq[(Int, Int)], bwBuses: Seq[(Float, Float)], attack: Float, filtAmp: Float, pan: (Float, Float))(implicit player: MusicPlayer): Unit = {
    val dur = duration
    val outBus = 20

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(outBus)
      .dur(dur)
      .ampBus.control(ar(dur, attack, (0f, 1f, 0f)))
      .buildInstruments()

    val filters = noiseFilters.indices.flatMap {
      index =>
        filterRejectReplaceInstrument
          .addAction(TAIL_ACTION)
          .in(outBus)
          .dur(dur)
          .ampBus.control(ar(dur, attack, (0f, filtAmp, 0f)))
          .bwBus.control(line(dur, bwBuses(index)._1, bwBuses(index)._2))
          .freqBus.control(line(dur, overSpectrum(noiseFilters(index)._1), overSpectrum(noiseFilters(index)._2)))
          .buildInstruments()
    }.toSeq

    val (startPan, endPan) = pan
    val pan1 = panInstrument
      .addAction(TAIL_ACTION)
      .in(outBus)
      .out(0)
      .dur(dur)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(time), noise ++ filters ++ pan1)
  }

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

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(outBus)
      .out(note.effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(time),
      pulse ++ highPass ++ lowPass ++ volume ++ whiteNoise ++ filterLower ++ filterUpper ++ lowerNote ++ upperNote ++ pan ++ effect)

  }

  case class PulseNote(pulseStartTime: Float, pulseEndTime: Float, nrOfPulses: Float,
                       lowerControl: ControlInstrumentBuilder, upperControl: ControlInstrumentBuilder,
                       pulseVolumeControl: ControlInstrumentBuilder,
                       lowerPulseVolumeControl: ControlInstrumentBuilder,
                       upperPulseVolumeControl: ControlInstrumentBuilder,
                       lowerNoiseVolumeControl: ControlInstrumentBuilder,
                       upperNoiseVolumeControl: ControlInstrumentBuilder,
                       panControl: ControlInstrumentBuilder, soundBus: Int = 16, effectBus: Int = 19) {

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
    notes.indices.foreach {
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

    def make: Seq[PulseNote] =
      pulse.indices.map {
        i =>
          PulseNote(
            underSpectrum(startPulse(i)), underSpectrum(endPulse(i)), nbrOfPulses(i),
            line(1f, overSpectrum(lowerStart(i)), overSpectrum(lowerEnd(i))),
            line(1f, overSpectrum(upperStart(i)), overSpectrum(upperEnd(i))),
            pulseVolumeControl(i), lowerPulseVolumeControl(i), upperPulseVolumeControl(i),
            lowerNoiseVolumeControl(i), upperNoiseVolumeControl(i), panControl(i), soundBus)
      }
  }

  def effect1(implicit player: MusicPlayer): Unit = {
    val bus = 19
    val dur = 70

    val volume = monoVolumeInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(23)
      .ampBus.control(line(dur, 0.3f, 0.5f))
      .buildInstruments()

    val delay = monoDelayReplaceInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(23)
      .delayBus.control(ar(dur, 0.3f, (0.03f, 0.055f, 0.02f), nodeId = EFFECT), sine(dur, 0.05f, 0.05f, 0.001f, 0.001f, nodeId = EFFECT))
      .maxDelay(0.06f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(23)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.02f, 0.01f, nodeId = EFFECT), sine(dur, 0.06f, 0.05f, 0.05f, 0.06f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.007f, 0.009f, nodeId = EFFECT), sine(dur, 0.007f, 0.003f, 0.01f, 0.008f, nodeId = EFFECT))
      .maxDelay(0.1f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(23)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.03f, 0.02f, nodeId = EFFECT), sine(dur, 0.09f, 0.05f, 0.05f, 0.09f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.01f, 0.009f, nodeId = EFFECT))
      .maxDelay(0.01f)
      .buildInstruments()

    val pan = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(23)
      .out(0)
      .panBus.control(line(dur, 0.1f, -0.1f, EFFECT))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(0), volume ++ delay ++ combFilter ++ allpassFilter ++ pan)
  }

  def roomEffect(implicit player: MusicPlayer): Unit = {
    val dur = 70
    val reverb = gverbInstrument
      .nodeId(ROOM_EFFECT)
      .addAction(TAIL_ACTION)
      .in(0)
      .out(0)
      .dur(dur)
      .roomSize(5)
      .revTime(0.6f)
      .damping(0.62f)
      .inputBw(0.48f)
      .spread(15)
      .earlyLevel(-11)
      .tailLevel(-13)
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(0), reverb)
  }

  def attackVolume(peak: Float) = ar(1, 0.3f, (0.0f, peak, 0.0f))
  def decayVolume(peak: Float) = ar(1, 0.7f, (0.0f, peak, 0.0f))
  def middleVolume(peak: Float) = ar(2, 0.5f, (0.0f, peak, 0.0f))

  val mostLefToSlightLeft = line(1f, -1f, -0.5f)

  val slightLeftToSlightRight = line(1f, -0.5f, 0.5f)

  val slightRightToMostRight = line(1f, 0.5f, 1f)

  def makeLowPulseMelody(): Seq[PulseNote] = {
    val pulseVolumes = Seq(middleVolume(0.05f), decayVolume(0.12f), attackVolume(0.07f), middleVolume(0.15f), decayVolume(0.07f), attackVolume(0.05f))
    val lowerPulseNoteVolumes = Seq(attackVolume(0.008f), middleVolume(0.01f), decayVolume(0.02f), attackVolume(0.01f), middleVolume(0.008f), decayVolume(0.02f))
    val upperPulseNoteVolumes = Seq(decayVolume(0.008f), attackVolume(0.01f), middleVolume(0.02f), decayVolume(0.01f), attackVolume(0.008f), middleVolume(0.02f))
    val lowerNoiseVolumes = Seq(attackVolume(0.6f), middleVolume(0.4f), decayVolume(0.3f), attackVolume(0.2f), middleVolume(0.4f), decayVolume(0.2f))
    val upperNoiseVolumes = Seq(decayVolume(0.6f), attackVolume(0.4f), middleVolume(0.3f), decayVolume(0.2f), attackVolume(0.4f), middleVolume(0.2f))

    val lowPulseMelody = PulseMelodyData(
      pulse = Seq(13, 20, 13, 20, 13, 20),
      nbrOfPulses = Seq(21, 21, 8, 8, 13, 13),
      lower = Seq(3, 2, 3, 2, 3, 2),
      upper = Seq(4, 5, 4, 5, 4, 5),
      pulseVolumeControl = pulseVolumes,
      lowerPulseVolumeControl = lowerPulseNoteVolumes,
      upperPulseVolumeControl = upperPulseNoteVolumes,
      lowerNoiseVolumeControl = lowerNoiseVolumes,
      upperNoiseVolumeControl = upperNoiseVolumes,
      panControl = Seq(
        mostLefToSlightLeft, mostLefToSlightLeft.reverse,
        mostLefToSlightLeft, mostLefToSlightLeft.reverse,
        mostLefToSlightLeft, mostLefToSlightLeft.reverse)
    )
    lowPulseMelody.make
  }

  def makeHighPulseMelody(): Seq[PulseNote] = {
    val pulseVolumes = Seq(middleVolume(0.5f), decayVolume(0.7f), attackVolume(0.1f), middleVolume(0.7f), decayVolume(0.3f), attackVolume(0.5f))
    val lowerPulseNoteVolumes = Seq(attackVolume(0.008f), middleVolume(0.01f), decayVolume(0.02f), attackVolume(0.008f), middleVolume(0.01f), decayVolume(0.02f))
    val upperPulseNoteVolumes = Seq(decayVolume(0.008f), attackVolume(0.01f), middleVolume(0.02f), decayVolume(0.008f), attackVolume(0.01f), middleVolume(0.02f))
    val lowerNoiseVolumes = Seq(attackVolume(0.3f), middleVolume(0.4f), decayVolume(0.5f), attackVolume(0.4f), middleVolume(0.5f), decayVolume(0.3f))
    val upperNoiseVolumes = Seq(decayVolume(0.3f), attackVolume(0.4f), middleVolume(0.5f), decayVolume(0.4f), attackVolume(0.5f), middleVolume(0.3f))

    val highPulseMelody = PulseMelodyData(
      pulse = Seq(30, 36, 30, 36/*, 30, 36*/),
      nbrOfPulses = Seq(13, 13, 8, 8, 5, 5),
      lower = Seq(60, 63, 60, 63, 60, 63),
      upper = Seq(70, 64, 70, 64, 70, 64),
      pulseVolumeControl = pulseVolumes,
      lowerPulseVolumeControl = lowerPulseNoteVolumes,
      upperPulseVolumeControl = upperPulseNoteVolumes,
      lowerNoiseVolumeControl = lowerNoiseVolumes,
      upperNoiseVolumeControl = upperNoiseVolumes,
      panControl = Seq(
        slightRightToMostRight.reverse, slightRightToMostRight,
        slightRightToMostRight.reverse, slightRightToMostRight,
        slightRightToMostRight.reverse, slightRightToMostRight),
      soundBus = 17
    )

    highPulseMelody.make
  }

  def makeMiddlePulseMelody(): Seq[PulseNote] = {
    val pulseVolumes = Seq(middleVolume(0.2f), decayVolume(0.6f), attackVolume(0.4f), middleVolume(0.6f), decayVolume(0.4f), attackVolume(0.2f))
    val lowerPulseNoteVolumes = Seq(attackVolume(0.02f), middleVolume(0.01f), decayVolume(0.008f), attackVolume(0.02f), middleVolume(0.02f), decayVolume(0.008f))
    val upperPulseNoteVolumes = Seq(decayVolume(0.02f), attackVolume(0.01f), middleVolume(0.008f), decayVolume(0.02f), attackVolume(0.02f), middleVolume(0.008f))
    val lowerNoiseVolumes = Seq(attackVolume(0.3f), middleVolume(0.4f), decayVolume(0.5f), attackVolume(0.3f), middleVolume(0.4f), decayVolume(0.5f))
    val upperNoiseVolumes = Seq(decayVolume(0.3f), attackVolume(0.4f), middleVolume(0.5f), decayVolume(0.3f), attackVolume(0.4f), middleVolume(0.5f))

    val middlePulseMelody = PulseMelodyData(
      pulse = Seq(21, 23, 21, 23),
      nbrOfPulses = Seq(21, 21, 13, 13),
      lower = Seq(30, 25, 30, 25),
      upper = Seq(40, 35, 40, 35),
      pulseVolumeControl = pulseVolumes,
      lowerPulseVolumeControl = lowerPulseNoteVolumes,
      upperPulseVolumeControl = upperPulseNoteVolumes,
      lowerNoiseVolumeControl = lowerNoiseVolumes,
      upperNoiseVolumeControl = upperNoiseVolumes,
      panControl = Seq(
        slightLeftToSlightRight, slightLeftToSlightRight.reverse,
        slightLeftToSlightRight, slightLeftToSlightRight.reverse,
        slightLeftToSlightRight, slightLeftToSlightRight.reverse
      ),
      soundBus = 18
    )

    middlePulseMelody.make
  }

  def forthMovement(): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    val lowPulseNotes = makeLowPulseMelody()

    val highPulseNotes = makeHighPulseMelody()

    val middlePulseNotes = makeMiddlePulseMelody()

      player.startPlay()

    setupNodes(player)

    effect1
    //roomEffect

    val noiseFilter1 = Seq((3, 2), (40, 35) , (64, 70))
    val noiseBw1 = Seq((.01f, .01f), (.0001f, .0001f), (.0005f, .0002f))

    val noiseFilter2 = Seq((5, 4), (25, 30), (60, 63))
    val noiseBw2 = Seq((.01f, .05f), (.0002f, .0001f), (.00015f, .00030f))

    playNoise(0f, 80, noiseFilter1, noiseBw1, 0.3f, 0.2f, (-.5f, .5f))
    playNoise(0f, 80, noiseFilter2, noiseBw2, 0.7f, 0.4f, (.5f, -.5f))

    playPulseNotes(lowPulseNotes, 0f)
    playPulseNotes(middlePulseNotes, (1f / underSpectrum(21)) * 3)
    playPulseNotes(highPulseNotes, (1f / underSpectrum(30)) * 8)

    Thread.sleep(5000)
  }

  def main(args: Array[String]): Unit = {
    forthMovement()
  }
}
