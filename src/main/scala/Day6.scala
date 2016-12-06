import scala.io.Source

object Day6 extends App {

  def selectChar[B](chars: List[Char], by: Int => Int): Char = {
    val freqMap = chars.foldLeft(Map.empty[Char, Int].withDefaultValue(0)) {
      case (frequencies, ch) => frequencies.updated(ch, frequencies(ch) + 1)
    }

    freqMap
      .toList
      .sortBy { case (_, frequency) => by(frequency) }
      .map { case (ch, _) => ch }
      .head
  }

  def mostFrequent(chars: List[Char]): Char =
    selectChar(chars, frequency => -frequency)

  def leastFrequent(chars: List[Char]): Char =
    selectChar(chars, frequency => frequency)

  val input = Source.fromResource("day6.txt")
  val lines = input.getLines().toList

  val firstResult = lines.transpose.map(mostFrequent).mkString
  println(s"The first decoded message is $firstResult")

  val secondResult = lines.transpose.map(leastFrequent).mkString
  println(s"The first decoded message is $secondResult")
}
