package music

import java.lang.Float
import java.{lang => jl}

import music.Instruments._
import net.soundmining.Spectrum._

/**
 * Main class for the piece
 */
object Piece {
  import Movement2._

  val overSpectrum = makeSpectrum(40, phi, 150)
  val underSpectrum = makeInvertedSpectrum(40, phi, 150)

  val melody =
    Seq(3, 2, 20, 21, 7, 14, 24, 15, 8, 9, 6, 11, 23, 18)

  def makeSeqWithIndex[T](seq: Seq[T]) = {
    0 until seq.size map {
      i => (i, seq(i))
    }
  }

  def main(args: Array[String]): Unit = {
    //println(s"overSpectrum")
    //makeSeqWithIndex(overSpectrum).foreach { case (i, v) => println(s"$i: $v")}
    //println(s"underSpectrum")
    //makeSeqWithIndex(underSpectrum).foreach { case (i, v) => println(s"$i: $v")}

    secondMovement()
  }
}
