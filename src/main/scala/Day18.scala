import scala.annotation.tailrec

object Day18 extends App {

  sealed trait Tile
  case object Safe extends Tile
  case object Trap extends Tile

  case class Row(tiles: Vector[Tile]) {
    def numberOfSafeTiles: Int = tiles.collect { case Safe => Safe }.length

    def next(): Row = {
      val padded = Safe +: tiles :+ Safe
      val nextTiles = padded.sliding(3).foldLeft(Vector.empty[Tile]) {
        case (result, Vector(Trap, Trap, Safe)) => result :+ Trap
        case (result, Vector(Safe, Trap, Trap)) => result :+ Trap
        case (result, Vector(Trap, Safe, Safe)) => result :+ Trap
        case (result, Vector(Safe, Safe, Trap)) => result :+ Trap
        case (result, _) => result :+ Safe
      }
      Row(nextTiles)
    }

    override def toString: String =
      tiles.map {
        case Safe => '.'
        case Trap => '^'
      }.mkString
  }

  object Row{
    def fromString(input: String): Row = {
      val tiles = input.map {
        case '.' => Safe
        case '^' => Trap
        case c: Char => throw new IllegalArgumentException(s"Input contains invalid character: $c")
      }
      Row(tiles.toVector)
    }
  }

  def calculateMap(initialRow: Row): Stream[Row] = {
    Stream.iterate(initialRow)(_.next())
  }

  def printMap(map: Seq[Row]): Unit = {
    map.foreach(println)
  }

  println("Example 1:")
  printMap(calculateMap(Row.fromString("..^^.")).take(3))

  println()
  println("Example 2:")
  printMap(calculateMap(Row.fromString(".^^.^.^^^^")).take(10))

  println()
  println("First:")
  val input = "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^"
  val map1 = calculateMap(Row.fromString(input))
  val safeCount1 = map1.map(_.numberOfSafeTiles).take(40).sum
  println(s"Number of safe tiles: $safeCount1")

  println("Second:")
  val map2 = calculateMap(Row.fromString(input))
  val safeCount2 = map2.map(_.numberOfSafeTiles).take(400000).sum
  println(s"Number of safe tiles: $safeCount2")
}
