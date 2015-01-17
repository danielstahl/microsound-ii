package music

import music.Instruments._
import music.Utils._

/**
 * The second movement
 */
object Movement2 {
  import music.Instruments.ARControlInstrumentBuilder._
  import music.Instruments.ASRControlInstrumentBuilder._
  import music.Instruments.LineControlInstrumentBuilder._
  import music.Instruments.SineControlReplaceInstrumentBuilder._
  import Piece._

  def makePulse(dur: Float, startPan: Float, endPan: Float): Seq[Seq[Object]] = {
    val pulse = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(16)
      .dur(dur)
      .freqBus.control(line(dur, underSpectrum(149), underSpectrum(98)))
      .widthBus.control(line(dur, underSpectrum(48), underSpectrum(34)))
      .ampBus.control(line(dur, 0.1f, 0.5f))
      .buildInstruments()

    val pulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(16)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    pulse ++ pulsePan
  }

  def makePulseFilter1(dur: Float, soundBus: Int, startPan: Float, endPan: Float): Seq[Seq[Object]] = {
    val pulseFilter = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16)
      .out(soundBus)
      .dur(dur)
      .ampBus.control(line(dur, 0.004f, 0.0007f))
      .freqBus.control(line(dur, overSpectrum(5), overSpectrum(6)))
      .bwBus.control(line(dur, 0.0000001f, 0.00000001f))
      .buildInstruments()

    val pulseFilterDelay = monoDelayReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .delayBus.control(ar(dur, 0.3f, (0.1f, 0.15f, 0.04f)), sine(dur, underSpectrum(48), underSpectrum(48), 0.001f, 0.001f))
      .maxDelay(0.2f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.9f), sine(dur, 0.6f, 0.5f, 0.05f, 0.06f))
      .delayBus.control(line(dur, 0.07f, 0.09f), sine(dur, 0.7f, 0.3f, 0.01f, 0.008f))
      .maxDelay(0.1f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.3f, 1.4f), sine(dur, 0.09f, 0.1f, 0.05f, 0.09f))
      .delayBus.control(line(dur, 0.01f, 0.009f))
      .maxDelay(0.01f)
      .buildInstruments()

    val filterPulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(soundBus)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    pulseFilter ++ pulseFilterDelay ++ combFilter ++ allpassFilter ++ filterPulsePan
  }

  def makePulseFilter2(dur: Float, soundBus: Int, startPan: Float, endPan: Float): Seq[Seq[Object]] = {
    val pulseFilter = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16)
      .out(soundBus)
      .dur(dur)
      .ampBus.control(line(dur, 0.003f, 0.001f))
      .freqBus.control(line(dur, overSpectrum(22), overSpectrum(10)))
      .bwBus.control(line(dur, 0.0000001f, 0.00000001f))
      .buildInstruments()

    val pulseFilterDelay = monoDelayReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .delayBus.control(ar(dur, 0.3f, (0.01f, 0.1f, 0.02f)), sine(dur, underSpectrum(48), underSpectrum(48), 0.001f, 0.001f))
      .maxDelay(0.2f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.2f, 1.3f), sine(dur, underSpectrum(40), underSpectrum(48), 0.05f, 0.06f))
      .delayBus.control(line(dur, 0.1f, 0.2f), sine(dur, 0.7f, 0.3f, 0.01f, 0.008f))
      .maxDelay(0.3f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.3f, 1.4f))
      .delayBus.control(line(dur, 0.15f, 0.2f), sine(dur, 0.09f, 0.1f, 0.05f, 0.09f))
      .maxDelay(0.2f)
      .buildInstruments()

    val filterPulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(soundBus)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    pulseFilter ++ pulseFilterDelay ++ combFilter ++ allpassFilter ++ filterPulsePan
  }

  def makePulseFilter3(dur: Float, soundBus: Int, startPan: Float, endPan: Float): Seq[Seq[Object]] = {
    val pulseFilter = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16)
      .out(soundBus)
      .dur(dur)
      .ampBus.control(line(dur, 0.006f, 0.003f))
      .freqBus.control(line(dur, overSpectrum(47), overSpectrum(57)))
      .bwBus.control(line(dur, 0.0000001f, 0.00000001f))
      .buildInstruments()

    val pulseFilterDelay = monoDelayReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .delayBus.control(ar(dur, 0.3f, (0.11f, 0.15f, 0.12f)), sine(dur, 0.1f, 0.2f, 0.01f, 0.01f))
      .maxDelay(0.15f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.3f), sine(dur, 0.2f, 0.1f, 0.03f, 0.02f))
      .delayBus.control(line(dur, 0.12f, 0.1f), sine(dur, 0.07f, 0.04f, 0.008f, 0.01f))
      .maxDelay(0.15f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.8f))
      .delayBus.control(line(dur, 0.1f, 0.15f), sine(dur, 0.02f, 0.04f, 0.01f, 0.02f))
      .maxDelay(0.15f)
      .buildInstruments()

    val filterPulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(soundBus)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    pulseFilter ++ pulseFilterDelay ++ combFilter ++ allpassFilter ++ filterPulsePan
  }


  def makeNoise(dur: Float, audioBus: Int): Seq[Seq[Object]] = {
    val whiteNoise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .out(audioBus)
      .ampBus.control(asr(dur, (0.0f, 0.1f, 0.15f, 0.0f), (0.01f, 10f, 0.2f)))
      .buildInstruments()

    whiteNoise
  }

  def makeNoiseFilter1(dur: Float, audioInBus: Int, audioOutBus: Int, startPan: Float, endPan: Float): Seq[Seq[Object]] = {
    val rejectFilter = filterRejectInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.01f, 0.001f))
      .freqBus.control(line(dur, overSpectrum(5), overSpectrum(6)))
      .buildInstruments()

    val filter = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(line(dur, overSpectrum(5), overSpectrum(6)))
      .buildInstruments()

    val noisePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioOutBus)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    rejectFilter ++ filter ++ noisePan
  }

  def makeNoiseFilter2(dur: Float, audioInBus: Int, audioOutBus: Int, startPan: Float, endPan: Float): Seq[Seq[Object]] = {
    val rejectFilter = filterRejectInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.5f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.01f, 0.001f))
      .freqBus.control(line(dur, overSpectrum(22), overSpectrum(10)))
      .buildInstruments()

    val filter = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.5f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(line(dur, overSpectrum(22), overSpectrum(10)))
      .buildInstruments()

    val noisePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioOutBus)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    rejectFilter ++ filter ++ noisePan
  }


  def makeNoiseFilter3(dur: Float, audioInBus: Int, audioOutBus: Int, startPan: Float, endPan: Float): Seq[Seq[Object]] = {
    val rejectFilter = filterRejectInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.001f, 0.0001f))
      .freqBus.control(line(dur, overSpectrum(47), overSpectrum(57)))
      .buildInstruments()

    val filter = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(line(dur, overSpectrum(47), overSpectrum(57)))
      .buildInstruments()

    val noisePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioOutBus)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()

    rejectFilter ++ filter ++ noisePan
  }

  /**
   * Microsound II part 2
   * This is what will be the second part of the piece Microsound II. It is
   * homage to Stockhausen and his piece Kontakte.
   *
   *
   *
   * The second movement.
   * The idea is to have two main layers
   * 1. A slow pulse with a slowly evolving freq (about 0.5/second)
   * and width (0.5 - 0.1)
   * 2. A noise that crossfade from white to pink
   * with a slowly moving filter.
   */
  def secondMovement(): Unit = {
    BusGenerator.reset()
    val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    val dur = overSpectrum(1)

    setupNodes(player)

    val messages =
      makePulse(dur, 0.33f, -0.66f) ++
        makePulseFilter1(dur, 17, -1f, 0.66f) ++
        makePulseFilter2(dur, 18, -0.33f, 0.33f) ++
        makePulseFilter3(dur, 19, 1f, -0.33f) ++
        makeNoise(dur, 20) ++
        makeNoiseFilter1(dur, 20, 21, 0.66f, 0f) ++
        makeNoiseFilter2(dur, 20, 22, 0f, -1f) ++
        makeNoiseFilter3(dur, 20, 23, -0.66f, 1f)
    player.sendNew(absoluteTimeToMillis(0f), messages.toSeq: _*)

    Thread.sleep(1000)
  }
}
