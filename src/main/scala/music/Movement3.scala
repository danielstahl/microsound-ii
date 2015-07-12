package music

import music.Instruments.LineControlInstrumentBuilder._
import music.Instruments.SineControlReplaceInstrumentBuilder._
import music.Instruments._
import music.Movement3.SubtractiveNote
import music.Piece._
import net.soundmining.Instrument._
import net.soundmining.Melody._
import net.soundmining.{ControlInstrumentBuilder, BusGenerator, MusicPlayer}
import net.soundmining.Utils._
import ARControlInstrumentBuilder._
import ASRControlInstrumentBuilder._

/**
 * This is the third movement
 *
 * Long sweeping chords with noise or pulse as base. At leaast 5 notes in the chords.
 *
 */
object Movement3 {


  /**
   * @param startTime startTime in seconds
   * @param duration duration in seconds
   * @param amp overall amp
   * @param pan start and end pan
   * @param filters seq with startFreq, endFreq and attackPoint
   * @param attackPoint overall attackPoint
   */
  case class SubtractiveNote(startTime: Float, duration: Float, amp: Float, pan: (Float, Float), filters: Seq[(Float, Float, Float)], attackPoint: Float, bus: Int)

  case class PulseNote(startTime: Float, duration: Float, amp: Float, attackPoint: Float, pan: (Float, Float), pulseFreq: /*(Float, Float)*/ControlInstrumentBuilder, highPass: (Float, Float), lowPass: (Float, Float), bus: Int)

  def subractiveFreqs(freqs: Seq[(Int, Int, Float)]): Seq[(Float, Float, Float)] =
    freqs.map {
      case (start, end, ap) => (overSpectrum(start), overSpectrum(end), ap)
    }


  def pulseFreq(start: Int, end: Int): (Float, Float) =
    (underSpectrum(start), underSpectrum(end))

  def highPass(start: Int, end: Int): (Float, Float) =
    (overSpectrum(start), overSpectrum(end))

  def lowPass(start: Int, end: Int): (Float, Float) =
    (overSpectrum(start), overSpectrum(end))

  def pulse(note: PulseNote)(implicit player: MusicPlayer): Unit = {
    val bus = note.bus
    val start = note.startTime
    val dur = note.duration
    val amp = note.amp
    val attackPoint = note.attackPoint

    val (highPassStart, highPassEnd) = note.highPass
    val (lowPassStart, lowPassEnd) = note.lowPass
    val (panStart, panEnd) = note.pan

    val pulse = pulseInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .out(bus)
      .freqBus.control(note.pulseFreq)
      .ampBus.control(ar(dur, attackPoint, (0f, amp, 0f)))
      .widthBus.control(line(dur, 0.5f, 0.5f))
      .buildInstruments()

    val highPass = highPassReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .freqBus.control(line(dur, highPassStart, highPassEnd))
      .buildInstruments()

    val lowPass = lowPassReplaceInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .freqBus.control(line(dur, lowPassStart, lowPassEnd))
      .buildInstruments()

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, panStart, panEnd))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(22)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    println(s"Pulse start $start (${absoluteTimeToMillis(start)}) and duration $dur")
    player.sendNew(absoluteTimeToMillis(start), pulse ++ highPass ++ lowPass ++ pan ++ effect)
  }

  def subtractiveNoise(note: SubtractiveNote)(implicit player: MusicPlayer): Unit = {
    val bus = note.bus
    val start = note.startTime
    val dur = note.duration
    val attack = note.attackPoint
    val (panStart, panEnd) = note.pan
    val effectBus = 20

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .ampBus.control(ar(dur, attack, (0f, 1f, 0f)))
      .buildInstruments()

    val filters = note.filters.flatMap {
      case (startFreq, endFreq, attackPoint) =>
        filterReplaceInstrument
          .addAction(TAIL_ACTION)
          .in(bus)
          .dur(dur)
          .ampBus.control(ar(dur, attackPoint, (0f, 1f, 0f)))
          .bwBus.control(line(dur, 0.00000001f, 0.000000001f))
          .freqBus.control(line(dur, startFreq, endFreq))
          .buildInstruments()
    }

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, panStart, panEnd))
      .buildInstruments()

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(20)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    println(s"Subtractive start $start (${absoluteTimeToMillis(start)}) and duration $dur")

    player.sendNew(absoluteTimeToMillis(start), noise ++ filters ++ pan ++ effect)
  }

  def effectSubtractive(implicit player: MusicPlayer): Unit = {
    val bus = 20
    val dur = 65

    val volume = monoVolumeInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(21)
      .ampBus.control(line(dur, 0.2f, 0.1f))
      .buildInstruments()

    val delay = monoDelayReplaceInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(21)
      .delayBus.control(ar(dur, 0.3f, (0.3f, 0.55f, 0.2f), nodeId = EFFECT), sine(dur, underSpectrum(48), underSpectrum(48), 0.001f, 0.001f, nodeId = EFFECT))
      .maxDelay(0.5f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(21)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.9f, nodeId = EFFECT), sine(dur, 0.6f, 0.5f, 0.05f, 0.06f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.07f, 0.09f, nodeId = EFFECT), sine(dur, 0.7f, 0.3f, 0.01f, 0.008f, nodeId = EFFECT))
      .maxDelay(0.1f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(21)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.3f, 1.4f, nodeId = EFFECT), sine(dur, 0.09f, 0.1f, 0.05f, 0.09f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.01f, 0.009f, nodeId = EFFECT))
      .maxDelay(0.01f)
      .buildInstruments()

    val pan1 = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(21)
      .out(0)
      .panBus.control(line(dur, 0.5f, 1f, EFFECT))
      .buildInstruments()

    val pan2 = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(21)
      .out(0)
      .panBus.control(line(dur, -0.5f, -1f, EFFECT))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(0), volume ++ delay ++ combFilter ++ allpassFilter ++ pan1 ++ pan2)
  }

  def effectPulse(implicit player: MusicPlayer): Unit = {
    val bus = 22
    val dur = 70

    val volume = monoVolumeInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(23)
      .ampBus.control(line(dur, 0.1f, 0.2f))
      .buildInstruments()

    val delay = monoDelayReplaceInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(23)
      .delayBus.control(ar(dur, 0.3f, (0.02f, 0.035f, 0.01f), nodeId = EFFECT), sine(dur, 0.05f, 0.05f, 0.001f, 0.001f, nodeId = EFFECT))
      .maxDelay(0.04f)
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

    val pan1 = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(23)
      .out(0)
      .panBus.control(line(dur, 1f, 0.5f, EFFECT))
      .buildInstruments()

    val pan2 = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(23)
      .out(0)
      .panBus.control(line(dur, -1f, -0.5f, EFFECT))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(0), volume ++ delay ++ combFilter ++ allpassFilter ++ pan1 ++ pan2)
  }

  def thirdMovement(): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    val subtractiveNotes_Org = Seq(
      SubtractiveNote(
        startTime = 0f,
        duration = 1.5f,
        amp = 1f,
        pan = (-0.5f, -0.7f),
        filters = subractiveFreqs(Seq((25, 29, 0.2f), (35, 41, 0.4f), (57, 67, 0.5f), (76, 95, 0.6f), (101, 110, 0.8f))),
        attackPoint = 0.3f,
        bus = 17),
      SubtractiveNote(
        startTime = 0.1f,
        duration = 1.6f,
        amp = 1f,
        pan = (0.1f, 0.5f),
        filters = subractiveFreqs(Seq((25, 29, 0.8f), (35, 41, 0.6f), (57, 67, 0.5f), (76, 95, 0.4f), (101, 110, 0.2f))),
        attackPoint = 0.7f,
        bus = 18),

      SubtractiveNote(
        startTime = 2f,
        duration = 7.5f,
        amp = 1f,
        pan = (-0.99f, -0.1f),
        filters = subractiveFreqs(Seq((3, 2, 0.7f), (10, 20, 0.1f), (11, 40, 0.3f), (90, 50, 0.5f), (92, 90, 0.4f))),
        attackPoint = 0.7f,
        bus = 17),
      SubtractiveNote(
        startTime = 2.1f,
        duration = 7.6f,
        amp = 1f,
        pan = (0.99f, 0.1f),
        filters = subractiveFreqs(Seq((1, 2, 0.7f), (10, 20, 0.1f), (11, 40, 0.3f), (90, 50, 0.5f), (92, 120, 0.4f))),
        attackPoint = 0.3f,
        bus = 18),


      SubtractiveNote(
        startTime = 10f,
        duration = 5.5f,
        amp = 1f,
        pan = (-0.99f, -0.9f),
        filters = subractiveFreqs(Seq((5, 4, 0.7f), (6, 7, 0.1f), (11, 10, 0.3f), (12, 15, 0.5f), (17, 16, 0.9f))),
        attackPoint = 0.5f,
        bus = 17),
      SubtractiveNote(
        startTime = 10.1f,
        duration = 5.6f,
        amp = 1f,
        pan = (0.99f, 0.8f),
        filters = subractiveFreqs(Seq((5, 4, 0.7f), (6, 7, 0.1f), (11, 10, 0.3f), (12, 15, 0.5f), (17, 16, 0.9f))),
        attackPoint = 0.5f,
        bus = 18),

      SubtractiveNote(
        startTime = 16f,
        duration = 5.5f,
        amp = 1f,
        pan = (-0.99f, -0.9f),
        filters = subractiveFreqs(Seq((100, 95, 0.7f), (102, 103, 0.1f), (105, 110, 0.3f), (106, 120, 0.5f), (122, 121, 0.9f))),
        attackPoint = 0.5f,
        bus = 17),
      SubtractiveNote(
        startTime = 16.1f,
        duration = 5.6f,
        amp = 1f,
        pan = (0.99f, 0.8f),
        filters = subractiveFreqs(Seq((100, 95, 0.7f), (102, 103, 0.1f), (105, 110, 0.3f), (106, 120, 0.5f), (122, 121, 0.9f))),
        attackPoint = 0.5f,
        bus = 18),

      SubtractiveNote(
        startTime = 22f,
        duration = 5.5f,
        amp = 1f,
        pan = (-0.99f, -0.0f),
        filters = subractiveFreqs(Seq((1, 48, 0.7f), (30, 49, 0.1f), (60, 50, 0.3f), (90, 56, 0.5f), (120, 57, 0.9f))),
        attackPoint = 0.5f,
        bus = 17),
      SubtractiveNote(
        startTime = 22.1f,
        duration = 5.6f,
        amp = 1f,
        pan = (0.99f, 0.0f),
        filters = subractiveFreqs(Seq((1, 48, 0.7f), (30, 49, 0.1f), (60, 50, 0.3f), (90, 56, 0.5f), (120, 57, 0.9f))),
        attackPoint = 0.5f,
        bus = 18),


      SubtractiveNote(
        startTime = 27f,
        duration = 5.5f,
        amp = 1f,
        pan = (-0.01f, -0.99f),
        filters = subractiveFreqs(Seq((25, 2, 0.8f), (26, 15, 0.9f), (28, 45, 0.5f), (33, 59, 0.2f), (34, 93, 0.8f))),
        attackPoint = 0.5f,
        bus = 17),
      SubtractiveNote(
        startTime = 27.1f,
        duration = 5.6f,
        amp = 1f,
        pan = (0.01f, 0.99f),
        filters = subractiveFreqs(Seq((25, 2, 0.3f), (26, 15, 0.9f), (28, 45, 0.5f), (33, 59, 0.2f), (34, 93, 0.7f))),
        attackPoint = 0.5f,
        bus = 18)

    )

    //subtractiveNotes.foreach(subtractiveNoise)

    val pulseNotes_Org = Seq(
        PulseNote(
        startTime = 0f,
        duration = 10f,
        amp = 1f,
        attackPoint = 0.1f,
        pan = (-0.1f, -0.9f),
        pulseFreq = line(10, underSpectrum(0), underSpectrum(30)),
        highPass = highPass(95, 4),
        lowPass = lowPass(100, 6),
        bus = 19
      ),
      PulseNote(
        startTime = 0f,
        duration = 10.01f,
        amp = 1f,
        attackPoint = 0.1f,
        pan = (0.1f, 0.9f),
        pulseFreq = line(10.01f, underSpectrum(0), underSpectrum(30)),
        highPass = highPass(40, 4),
        lowPass = lowPass(50, 6),
        bus = 20
      ),

      PulseNote(
        startTime = 12f,
        duration = 7f,
        amp = 1f,
        attackPoint = 0.2f,
        pan = (0.1f, 0.5f),
        pulseFreq = asr(7f, (underSpectrum(0), underSpectrum(10), underSpectrum(20), underSpectrum(5)), (0.2f, 0.5f, 0.3f)),
        highPass = highPass(95, 4),
        lowPass = lowPass(100, 6),
        bus = 19),
      PulseNote(
        startTime = 12.01f,
        duration = 6.99f,
        amp = 1f,
        attackPoint = 0.2f,
        pan = (-0.9f, -0.1f),
        pulseFreq = asr(6.99f, (underSpectrum(0), underSpectrum(10), underSpectrum(20), underSpectrum(5)), (0.2f, 0.5f, 0.3f)),
        highPass = highPass(50, 4),
        lowPass = lowPass(70, 6),
        bus = 20),

      PulseNote(
        startTime = 20f,
        duration = 10f,
        amp = 1f,
        attackPoint = 0.3f,
        pan = (0.7f, 0.2f),
        pulseFreq = asr(10f, (underSpectrum(10), underSpectrum(5), underSpectrum(8), underSpectrum(0)), (0.3f, 0.4f, 0.3f)),
        highPass = highPass(4, 50),
        lowPass = lowPass(6, 70),
        bus = 19),
      PulseNote(
        startTime = 20.01f,
        duration = 9.99f,
        amp = 1f,
        attackPoint = 0.3f,
        pan = (-0.5f, -0.3f),
        pulseFreq = asr(9.99f, (underSpectrum(10), underSpectrum(5), underSpectrum(8), underSpectrum(0)), (0.3f, 0.4f, 0.3f)),
        highPass = highPass(4, 50),
        lowPass = lowPass(6, 70),
        bus = 20),

      PulseNote(
        startTime = 30f,
        duration = 3f,
        amp = 1f,
        attackPoint = 0.7f,
        pan = (-0.3f, 0.5f),
        pulseFreq =  ar(3, 0.5f, (underSpectrum(5), underSpectrum(2), underSpectrum(6))),
        highPass = highPass(4, 10),
        lowPass = lowPass(6, 20),
        bus = 19),
      PulseNote(
        startTime = 30.01f,
        duration = 2.99f,
        amp = 1f,
        attackPoint = 0.7f,
        pan = (-0.1f, 0.7f),
        pulseFreq =  ar(2.99f, 0.5f, (underSpectrum(5), underSpectrum(2), underSpectrum(6))),
        highPass = highPass(4, 10),
        lowPass = lowPass(6, 20),
        bus = 20)
    )

    effectSubtractive

    effectPulse

    val subtractiveNotesStartTimes =
      absolute(underSpectrum(7),
      Seq(underSpectrum(2), underSpectrum(20), underSpectrum(12), underSpectrum(1), underSpectrum(9), underSpectrum(2), underSpectrum(1)))

    val subtractiveNotes = Seq(
      // 2,3
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes.head,
        duration = underSpectrum(2),
        amp = 1f,
        pan = (-0.99f, -0.1f),
        filters = subractiveFreqs(Seq((3, 2, 0.7f), (10, 20, 0.1f), (11, 40, 0.3f), (90, 50, 0.5f), (92, 90, 0.4f))),
        attackPoint = 0.7f,
        bus = 16),
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes.head + 0.1f,
        duration = underSpectrum(2) + 0.1f,
        amp = 1f,
        pan = (0.99f, 0.1f),
        filters = subractiveFreqs(Seq((1, 2, 0.7f), (10, 20, 0.1f), (11, 40, 0.3f), (90, 50, 0.5f), (92, 120, 0.4f))),
        attackPoint = 0.3f,
        bus = 17),

    // pause
    // 0,1
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(2),
        duration = underSpectrum(12),
        amp = 1f,
        pan = (-0.5f, -0.7f),
        filters = subractiveFreqs(Seq((25, 29, 0.2f), (35, 41, 0.4f), (57, 67, 0.5f), (76, 95, 0.6f), (101, 110, 0.8f))),
        attackPoint = 0.3f,
        bus = 16),
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(2) + 0.1f,
        duration = underSpectrum(12) + 0.1f,
        amp = 1f,
        pan = (0.1f, 0.5f),
        filters = subractiveFreqs(Seq((25, 29, 0.8f), (35, 41, 0.6f), (57, 67, 0.5f), (76, 95, 0.4f), (101, 110, 0.2f))),
        attackPoint = 0.7f,
        bus = 17),

      // 4,5
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(3),
        duration = underSpectrum(2),
        amp = 1f,
        pan = (-0.99f, -0.9f),
        filters = subractiveFreqs(Seq((5, 4, 0.7f), (6, 7, 0.1f), (11, 10, 0.3f), (12, 15, 0.5f), (17, 16, 0.9f))),
        attackPoint = 0.5f,
        bus = 16),
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(3) + 0.1f,
        duration = underSpectrum(2) + 0.1f,
        amp = 1f,
        pan = (0.99f, 0.8f),
        filters = subractiveFreqs(Seq((5, 4, 0.7f), (6, 7, 0.1f), (11, 10, 0.3f), (12, 15, 0.5f), (17, 16, 0.9f))),
        attackPoint = 0.5f,
        bus = 17),

    // 6,7
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(4),
        duration = underSpectrum(9),
        amp = 1f,
        pan = (-0.99f, -0.9f),
        filters = subractiveFreqs(Seq((100, 95, 0.7f), (102, 103, 0.1f), (105, 110, 0.3f), (106, 120, 0.5f), (122, 121, 0.9f))),
        attackPoint = 0.5f,
        bus = 16),
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(4) + 0.1f,
        duration = underSpectrum(9) + 0.1f,
        amp = 1f,
        pan = (0.99f, 0.8f),
        filters = subractiveFreqs(Seq((100, 95, 0.7f), (102, 103, 0.1f), (105, 110, 0.3f), (106, 120, 0.5f), (122, 121, 0.9f))),
        attackPoint = 0.5f,
        bus = 17),

    //10,11
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(5),
        duration = underSpectrum(2),
        amp = 1f,
        pan = (-0.01f, -0.99f),
        filters = subractiveFreqs(Seq((25, 2, 0.8f), (26, 15, 0.9f), (28, 45, 0.5f), (33, 59, 0.2f), (34, 93, 0.8f))),
        attackPoint = 0.5f,
        bus = 16),
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(5) + 0.1f,
        duration = underSpectrum(2) + 0.6f,
        amp = 1f,
        pan = (0.01f, 0.99f),
        filters = subractiveFreqs(Seq((25, 2, 0.3f), (26, 15, 0.9f), (28, 45, 0.5f), (33, 59, 0.2f), (34, 93, 0.7f))),
        attackPoint = 0.5f,
        bus = 17),

    //8,9
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(6),
        duration = underSpectrum(1),
        amp = 1f,
        pan = (-0.99f, -0.0f),
        filters = subractiveFreqs(Seq((1, 48, 0.7f), (30, 49, 0.1f), (60, 50, 0.3f), (90, 56, 0.5f), (120, 57, 0.9f))),
        attackPoint = 0.5f,
        bus = 16),
      SubtractiveNote(
        startTime = subtractiveNotesStartTimes(6) + 0.1f,
        duration = underSpectrum(1) + 0.1f,
        amp = 1f,
        pan = (0.99f, 0.0f),
        filters = subractiveFreqs(Seq((1, 48, 0.7f), (30, 49, 0.1f), (60, 50, 0.3f), (90, 56, 0.5f), (120, 57, 0.9f))),
        attackPoint = 0.5f,
        bus = 17)
    )

    subtractiveNotes.foreach(subtractiveNoise)


    val pulseNotesstartTimes = absolute(0,
      Seq(underSpectrum(2), underSpectrum(5), underSpectrum(1), underSpectrum(4), underSpectrum(2))
    )


    val pulseNotes = Seq(
      // 0
      PulseNote(
        startTime = pulseNotesstartTimes.head,
        duration = underSpectrum(2),
        amp = 1f,
        attackPoint = 0.1f,
        pan = (-0.1f, -0.9f),
        pulseFreq = line(10, underSpectrum(0), underSpectrum(30)),
        highPass = highPass(95, 4),
        lowPass = lowPass(100, 6),
        bus = 18
      ),
      PulseNote(
        startTime = pulseNotesstartTimes.head,
        duration = underSpectrum(2) + 0.01f,
        amp = 1f,
        attackPoint = 0.1f,
        pan = (0.1f, 0.9f),
        pulseFreq = line(10.01f, underSpectrum(0), underSpectrum(30)),
        highPass = highPass(40, 4),
        lowPass = lowPass(50, 6),
        bus = 19
      ),
      // pause
      // 4,5
      PulseNote(
        startTime = pulseNotesstartTimes(2),
        duration = underSpectrum(1),
        amp = 1f,
        attackPoint = 0.3f,
        pan = (0.7f, 0.2f),
        pulseFreq = asr(underSpectrum(1), (underSpectrum(10), underSpectrum(5), underSpectrum(8), underSpectrum(0)), (0.3f, 0.4f, 0.3f)),
        highPass = highPass(4, 50),
        lowPass = lowPass(6, 70),
        bus = 18),
      PulseNote(
        startTime = pulseNotesstartTimes(2) + 0.01f,
        duration = underSpectrum(1) - 0.01f,
        amp = 1f,
        attackPoint = 0.3f,
        pan = (-0.5f, -0.3f),
        pulseFreq = asr(underSpectrum(1) - 0.01f, (underSpectrum(10), underSpectrum(5), underSpectrum(8), underSpectrum(0)), (0.3f, 0.4f, 0.3f)),
        highPass = highPass(4, 50),
        lowPass = lowPass(6, 70),
        bus = 19),

    // 6,7
      PulseNote(
        startTime = pulseNotesstartTimes(3),
        duration = underSpectrum(4),
        amp = 1f,
        attackPoint = 0.7f,
        pan = (-0.3f, 0.5f),
        pulseFreq =  ar(underSpectrum(4), 0.5f, (underSpectrum(5), underSpectrum(2), underSpectrum(6))),
        highPass = highPass(4, 10),
        lowPass = lowPass(6, 20),
        bus = 18),
      PulseNote(
        startTime = pulseNotesstartTimes(3) + 0.01f,
        duration = underSpectrum(4) - 0.01f,
        amp = 1f,
        attackPoint = 0.7f,
        pan = (-0.1f, 0.7f),
        pulseFreq =  ar(underSpectrum(4) - 0.01f, 0.5f, (underSpectrum(5), underSpectrum(2), underSpectrum(6))),
        highPass = highPass(4, 10),
        lowPass = lowPass(6, 20),
        bus = 19),

    // 2,3
      PulseNote(
        startTime = pulseNotesstartTimes(4),
        duration = underSpectrum(2),
        amp = 1f,
        attackPoint = 0.2f,
        pan = (0.1f, 0.5f),
        pulseFreq = asr(underSpectrum(2), (underSpectrum(0), underSpectrum(10), underSpectrum(20), underSpectrum(5)), (0.2f, 0.5f, 0.3f)),
        highPass = highPass(95, 4),
        lowPass = lowPass(100, 6),
        bus = 18),
      PulseNote(
        startTime = pulseNotesstartTimes(4),
        duration = underSpectrum(2) + 0.01f,
        amp = 1f,
        attackPoint = 0.2f,
        pan = (-0.9f, -0.1f),
        pulseFreq = asr(underSpectrum(2) + 0.01f, (underSpectrum(0), underSpectrum(10), underSpectrum(20), underSpectrum(5)), (0.2f, 0.5f, 0.3f)),
        highPass = highPass(50, 4),
        lowPass = lowPass(70, 6),
        bus = 19)
    )

    pulseNotes.foreach(pulse)

    Thread.sleep(5000)
  }

  def main(args: Array[String]): Unit = {
    thirdMovement()
  }
}
