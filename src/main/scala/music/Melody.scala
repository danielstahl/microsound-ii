package music

/**
 * Utils for working with melodies
 */
object Melody {
  def transpose(amount: Int, melody: Seq[Int]): Seq[Int] = {
    melody.map(_ + amount)
  }

  def inverse(melody: Seq[Int]): Seq[Int] = {
    val intervals = makeIntervals(melody)
    var result = Seq(intervals(0))
    var last = intervals(0)
    intervals.tail.foreach {
      item =>
        val tmp = last + (item * -1)
        last = tmp
        result = result ++ Seq(tmp)
    }
    result
  }


  def makeIntervals(melody: Seq[Int]): Seq[Int] = {
    var lastItem = melody(0)
    var result = Seq(melody(0))

    melody.tail.foreach {
      item =>
        val interval = item - lastItem
        lastItem = item
        result = result ++ Seq(interval)
    }
    result
  }


  def retrograde(melody: Seq[Int]): Seq[Int] = melody.reverse

  def concrete(indices: Seq[Int], spectrum: Seq[Float]): Seq[Float] = {
    indices.map(spectrum(_))
  }

}
