package music

import org.scalatest.FunSuite
import Spectrum._
/**
 * TestCode for
 */
class SpectrumSpec extends FunSuite {

  test("make spektrum") {
    assert(makeSpectrum(110, 2, 5) === Seq[Float](110.0f, 330.0f, 550.0f, 770.0f, 990.0f))
  }

  test("make inverted spektrum") {
    assert(makeInvertedSpectrum(110, 2f, 5) === Seq[Float](110.0f, 36.666668f, 22.0f, 15.714286f, 12.222222f))
  }
}