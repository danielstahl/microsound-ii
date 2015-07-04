package music

import music.Instruments.LineControlInstrumentBuilder._
import music.Instruments._
import music.Piece._
import net.soundmining.Instrument._
import net.soundmining.Melody._
import net.soundmining.{BusGenerator, MusicPlayer}
import net.soundmining.Utils._
import ARControlInstrumentBuilder._

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

  case class PulseNote(startTime: Float, duration: Float, amp: Float, attackPoint: Float, pan: (Float, Float), pulseFreq: (Float, Float), highPass: (Float, Float), lowPass: (Float, Float), bus: Int)

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

    val (pulseFreqStart, pulseFreqEnd) = note.pulseFreq
    val (highPassStart, highPassEnd) = note.highPass
    val (lowPassStart, lowPassEnd) = note.lowPass

    val pulse = pulseInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .out(bus)
      .freqBus.control(line(dur, pulseFreqStart, pulseFreqEnd))
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
      .panBus.control(line(dur, 0.1f, -0.1f))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(start), pulse ++ highPass ++ lowPass ++ pan)
  }

  def subtractiveNoise(note: SubtractiveNote)(implicit player: MusicPlayer): Unit = {
    val bus = note.bus
    val start = note.startTime
    val dur = note.duration
    val attack = note.attackPoint
    val (panStart, panEnd) = note.pan

    val noise = whiteNoiseInstrument
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .ampBus.control(ar(dur, attack, (0f, 1f, 0f)))
      .buildInstruments()

    val filters = note.filters.flatMap {
      case (start, end, attack) =>
        filterReplaceInstrument
          .addAction(TAIL_ACTION)
          .in(bus)
          .dur(dur)
          .ampBus.control(ar(dur, attack, (0f, 1f, 0f)))
          .bwBus.control(line(dur, 0.00000001f, 0.000000001f))
          .freqBus.control(line(dur, start, end))
          .buildInstruments()
    }

    val pan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, panStart, panEnd))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(start), noise ++ filters ++ pan)
  }

  def thirdMovement(): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    val subtractiveNotes = Seq(
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
        bus = 18)
    )

    //subtractiveNotes.foreach(subtractiveNoise)

    val pulseNotes = Seq(
        PulseNote(
        startTime = 0f,
        duration = 25,
        amp = 1f,
        attackPoint = 0.1f,
        pan = (0.1f, -0.1f),
        pulseFreq = pulseFreq(0, 30),
        highPass = highPass(95, 4),
        lowPass = lowPass(100, 6),
        bus = 16
      )
    )

    pulseNotes.foreach(pulse)

    Thread.sleep(5000)
  }

  def main(args: Array[String]): Unit = {
    thirdMovement()
  }
}
