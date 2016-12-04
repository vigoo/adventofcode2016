import scala.io.Source

object Day4 extends App {

  case class Room(encryptedName: String, sectorId: Int, checksum: String) {
    def charFrequency: Map[Char, Int] =
      encryptedName
        .filter(_ != '-')
        .foldLeft(Map.empty[Char, Int].withDefaultValue(0)) {
          case (result, char) => result.updated(char, result(char) + 1)
        }

    def charsOrderedByFrequency: List[Char] =
      charFrequency
        .toList
        .sortBy { case (char, freq) => -(freq*100 - char.getNumericValue) }
        .map { case (char, _) => char }

    def isValid: Boolean =
      charsOrderedByFrequency.take(5).mkString == checksum

    def decryptedName: String =
      encryptedName.map {
        case '-' => ' '
        case ch: Char => ((((ch - 'a') + sectorId) % ('z' - 'a' + 1)) + 'a').toChar
      }
  }

  val room = """(.+)-(\d+)\[(.+)\]$""".r

  def parseRoom(line: String): Room = {
    line match {
      case room(encryptedName, sectorId, checksum) =>
        Room(encryptedName, sectorId.toInt, checksum)
      case _ =>
        throw new IllegalArgumentException(s"Pattern match failed on $line")
    }
  }

  val input = Source.fromResource("day4.txt")
  val rooms = input.getLines().toList.map(parseRoom)
  val validRooms = rooms.filter(_.isValid)

  val sectorIdSum = validRooms.map(_.sectorId).sum
  println(s"Sum of the valid room's sector IDs: $sectorIdSum")

  println(validRooms.filter(_.decryptedName.startsWith("north")))
}
