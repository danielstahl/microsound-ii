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
 * The same theme as the first movement
 * Only long overlapping notes.
 */
object Movement3 {

  def playNote(start: Float, dur: Float, freq: Float, attack: Float)(implicit player: MusicPlayer): Unit = {
    println(s"start $start dur $dur attack $attack freq $freq")
    val pulse = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(0)
      .dur(dur)
      .freqBus.control(line(dur, freq, freq))
      .widthBus.control(line(dur, 0.5f, 0.5f))
      .ampBus.control(ar(dur, attack, (0.001f, 0.1f, 0.001f)))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(start), pulse)
  }

  def thirdMovement(): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    val concreteMelody = concrete(melody, overSpectrum)
    val concreteRhythm = concrete(melody, underSpectrum)
    val absoluteTime = absolute(0, concreteRhythm)
    val durations = concrete(transpose(-2, melody), underSpectrum)

    melody.indices.foreach {
      i =>
        playNote(absoluteTime(i), durations(i), concreteMelody(i), concreteRhythm(i) / durations(i))
    }
    Thread.sleep(5000)
  }

  def main(args: Array[String]): Unit = {
    thirdMovement()
  }
}
