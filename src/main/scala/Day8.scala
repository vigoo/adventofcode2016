import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {

  val width = 50
  val height = 6

  type ScreenBuffer = Array[Array[Boolean]]

  def emptyScreenBuffer: ScreenBuffer = {
    val result = new Array[Array[Boolean]](height)
    for (i <- 0 until height) {
      result(i) = new Array[Boolean](width)
    }
    result
  }

  final class Screen(pixels: ScreenBuffer = emptyScreenBuffer) {
    def rect(w: Int, h: Int): Unit = {
      for (y <- 0 until h) {
        for (x <- 0 until w) {
          pixels(y)(x) = true
        }
      }
    }

    @tailrec
    def rotateColumn(x: Int, by: Int): Unit = {
      if (by > 0) {
        val last = pixels(height-1)(x)
        for (y <- (height-2) to(0, -1)) {
          pixels(y+1)(x) = pixels(y)(x)
        }
        pixels(0)(x) = last
        rotateColumn(x, by - 1)
      }
    }

    @tailrec
    def rotateRow(y: Int, by: Int): Unit = {
      if (by > 0) {
        val last = pixels(y)(width-1)
        for (x <- (width-2) to(0, -1)) {
          pixels(y)(x+1) = pixels(y)(x)
        }
        pixels(y)(0) = last
        rotateRow(y, by - 1)
      }
    }

    def numberOfPixelsLit: Int = {
      val litPixels =
        for {
          y <- 0 until height
          x <- 0 until width
          if pixels(y)(x)
        } yield (x, y)
      litPixels.length
    }

    def dump(): Unit = {
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          if (pixels(y)(x)) {
            print('#')
          } else {
            print('-')
          }
        }
        println()
      }
    }
  }

  sealed trait Command
  case class Rect(w: Int, h: Int) extends Command
  case class RotateColumn(x: Int, by: Int) extends Command
  case class RotateRow(y: Int, by: Int) extends Command
  case class Unknown(line: String) extends Command

  def parseRectCommand(line: String): Option[Command] = {
    val rect = """rect (\d+)x(\d+)$""".r
    line match {
      case rect(w, h) => Some(Rect(w.toInt, h.toInt))
      case _ => None
    }
  }

  def parseRotateColumnCommand(line: String): Option[Command] = {
    val rotateCol = """rotate column x=(\d+) by (\d+)$""".r
    line match {
      case rotateCol(x, by) => Some(RotateColumn(x.toInt, by.toInt))
      case _ => None
    }
  }

  def parseRotateRowCommand(line: String): Option[Command] = {
    val rotateRow = """rotate row y=(\d+) by (\d+)$""".r
    line match {
      case rotateRow(y, by) => Some(RotateRow(y.toInt, by.toInt))
      case _ => None
    }
  }

  def parseCommand(line: String): Command =
    parseRectCommand(line).getOrElse(
      parseRotateColumnCommand(line).getOrElse(
        parseRotateRowCommand(line).getOrElse(
          Unknown(line))))

  val screen = new Screen()

  val input = Source.fromResource("day8.txt")
  val commands = input.getLines().toList.map(parseCommand)

  commands.foreach {
    case Rect(w, h) => screen.rect(w, h)
    case RotateColumn(x, by) => screen.rotateColumn(x, by)
    case RotateRow(y, by) => screen.rotateRow(y, by)
    case Unknown(line) => println(s"Unknown command: $line")
  }

  screen.dump()
  println(s"Number of lit pixels: ${screen.numberOfPixelsLit}")
}
