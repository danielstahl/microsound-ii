package music

/**
 * Utils
 */
object Utils {
  def absoluteTimeToMillis(time: Float): Long = (time * 1000).round.toLong
}