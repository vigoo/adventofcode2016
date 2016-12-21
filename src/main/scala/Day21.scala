import scala.collection.mutable
import scala.io.Source

object Day21 extends App {

  sealed trait Operation
  case class SwapPosition(x: Int, y: Int) extends Operation
  case class SwapLetter(x: Char, y: Char) extends Operation
  case class RotateLeft(by: Int) extends Operation
  case class RotateRight(by: Int) extends Operation
  case class RotateTo(x: Char) extends Operation
  case class ReversePositions(x: Int, y: Int) extends Operation
  case class Move(x: Int, y: Int) extends Operation

  class Scrambler(initial: String) {
    private val builder = new mutable.StringBuilder(initial)

    def perform(operation: Operation): Unit = {
      operation match {
        case SwapPosition(x, y) =>
          val ch = builder(x)
          builder(x) = builder(y)
          builder(y) = ch

        case SwapLetter(x, y) =>
          val indexOfX = builder.indexOf(x)
          val indexOfY = builder.indexOf(y)
          val ch = builder(indexOfX)
          builder(indexOfX) = builder(indexOfY)
          builder(indexOfY) = ch

        case RotateLeft(by) =>
          val safeBy = by % builder.length
          val first = builder.substring(0, safeBy)
          val second = builder.substring(safeBy)
          builder.clear()
          builder.append(second)
          builder.append(first)

        case RotateRight(by) =>
          val l = builder.length
          val safeBy = by % l
          val first = builder.substring(l - safeBy)
          val second = builder.substring(0, l - safeBy)
          builder.clear()
          builder.append(first)
          builder.append(second)

        case RotateTo(x) =>
          val rotateBy: Int = calculateRotateTo(x)
          perform(RotateRight(rotateBy))

        case ReversePositions(x, y) =>
          val slice = builder.substring(x, y+1)
          builder.replace(x, y+1, slice.reverse)

        case Move(x, y) =>
          val ch = builder(x)
          builder.deleteCharAt(x)
          builder.insert(y, ch)
      }
    }

    private def calculateRotateTo(x: Char) = {
      val indexOfX = builder.indexOf(x)
      1 + indexOfX + (if (indexOfX >= 4) 1 else 0)
    }

    def revert(operation: Operation): Operation = {
      operation match {
        case SwapPosition(x, y) => SwapPosition(x, y)
        case SwapLetter(x, y) => SwapLetter(x, y)
        case RotateLeft(by) => RotateRight(by)
        case RotateRight(by) => RotateLeft(by)
        case RotateTo(x) =>
          (1 to builder.length).find { by =>
            val temporaryScrambler = new Scrambler(builder.result())
            temporaryScrambler.perform(RotateLeft(by))
            (temporaryScrambler.calculateRotateTo(x) % builder.length) == (by % builder.length)
          }.map(RotateLeft).get
        case ReversePositions(x, y) => ReversePositions(x, y)
        case Move(x, y) => Move(y, x)
      }
    }

    def result(): String = builder.result()
  }

  object Parser {
    private val rotateRight = """rotate right (\d+) steps?""".r
    private val rotateLeft = """rotate left (\d+) steps?""".r
    private val swapLetter = """swap letter ([a-z]) with letter ([a-z])""".r
    private val swapPosition = """swap position (\d+) with position (\d+)""".r
    private val reversePositions = """reverse positions (\d+) through (\d+)""".r
    private val move = """move position (\d+) to position (\d+)""".r
    private val rotateTo = """rotate based on position of letter ([a-z])""".r

    def apply(line: String): Operation = {
      line match {
        case rotateRight(by) => RotateRight(by.toInt)
        case rotateLeft(by) => RotateLeft(by.toInt)
        case swapLetter(x, y) => SwapLetter(x(0), y(0))
        case swapPosition(x, y) => SwapPosition(x.toInt, y.toInt)
        case reversePositions(x, y) => ReversePositions(x.toInt, y.toInt)
        case move(x, y) => Move(x.toInt, y.toInt)
        case rotateTo(x) => RotateTo(x(0))
        case _ => throw new IllegalArgumentException(s"Could not parse $line")
      }
    }
  }

  def verifyExample(scrambler: Scrambler, operation: Operation, expectedResult: String): Unit = {
    val before = scrambler.result()
    scrambler.perform(operation)
    assert(scrambler.result() == expectedResult)

    scrambler.perform(scrambler.revert(operation))
    assert(scrambler.result() == before)

    scrambler.perform(operation)
  }

  val example = new Scrambler("abcde")
  verifyExample(example, SwapPosition(4, 0), "ebcda")
  verifyExample(example, SwapLetter('d', 'b'), "edcba")
  verifyExample(example, ReversePositions(0, 4), "abcde")
  verifyExample(example, RotateLeft(1), "bcdea")
  verifyExample(example, Move(1, 4), "bdeac")
  verifyExample(example, Move(3, 0), "abdec")
  verifyExample(example, RotateTo('b'), "ecabd")
  verifyExample(example, RotateTo('d'), "decab")

  val operations = Source.fromResource("day21.txt").getLines.map(Parser.apply).toList
  val scrambler1 = new Scrambler("abcdefgh")
  operations.foreach(scrambler1.perform)
  println(s"First result is ${scrambler1.result()}")

  val scrambler2 = new Scrambler("fbgdceah")
  operations.reverse.foreach { op =>
    scrambler2.perform(scrambler2.revert(op))
  }
  println(s"Second result is ${scrambler2.result()}")
}
