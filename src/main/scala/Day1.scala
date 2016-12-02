import scala.annotation.tailrec
import scala.util.Try

object Day1 extends App {
  sealed trait Step
  case class Left(blocks: Int) extends Step
  case class Right(blocks: Int) extends Step

  object Step {
    def parse(raw: String): Option[Step] = {
      raw.headOption.flatMap { prefix =>
        Try {
          raw.tail.toInt
        }.toOption.flatMap { blocks =>
          prefix match {
            case 'R' => Some(Right(blocks))
            case 'L' => Some(Left(blocks))
            case _ => None
          }
        }
      }
    }
  }

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

  case class Coords(x: Int, y: Int) {
    @tailrec
    final def seriesBy(delta: Coords, result: Vector[Coords] = Vector.empty): Vector[Coords] = {
      delta match {
        case Coords(dx, dy) if dx < 0 =>
          seriesBy(Coords(dx + 1, dy), Coords(x + dx, y) +: result)
        case Coords(dx, dy) if dx > 0 =>
          seriesBy(Coords(dx - 1, dy), Coords(x + dx, y) +: result)
        case Coords(dx, dy) if dx == 0 && dy < 0 =>
          seriesBy(Coords(dx, dy + 1), Coords(x + dx, y + dy) +: result)
        case Coords(dx, dy) if dx == 0 && dy > 0 =>
          seriesBy(Coords(dx, dy - 1), Coords(x + dx, y + dy) +: result)
        case _ =>
          result
      }
    }
  }

  case class State(pos: Coords, dir: Direction, positionHistory: Vector[Coords]) {
    def to(delta: Coords, newDirection: Direction): State = {
      State(Coords(pos.x + delta.x, pos.y + delta.y), newDirection, positionHistory ++ pos.seriesBy(delta))
    }
  }

  val input = "R3, L2, L2, R4, L1, R2, R3, R4, L2, R4, L2, L5, L1, R5, R2, R2, L1, R4, R1, L5, L3, R4, R3, R1, L1, L5, L4, L2, R5, L3, L4, R3, R1, L3, R1, L3, R3, L4, R2, R5, L190, R2, L3, R47, R4, L3, R78, L1, R3, R190, R4, L3, R4, R2, R5, R3, R4, R3, L1, L4, R3, L4, R1, L4, L5, R3, L3, L4, R1, R2, L4, L3, R3, R3, L2, L5, R1, L4, L1, R5, L5, R1, R5, L4, R2, L2, R1, L5, L4, R4, R4, R3, R2, R3, L1, R4, R5, L2, L5, L4, L1, R4, L4, R4, L4, R1, R5, L1, R1, L5, R5, R1, R1, L3, L1, R4, L1, L4, L4, L3, R1, R4, R1, R1, R2, L5, L2, R4, L1, R3, L5, L2, R5, L4, R5, L5, R3, R4, L3, L3, L2, R2, L5, L5, R3, R4, R3, R4, R3, R1"
  val steps = input.split(',').map(_.trim).map(Step.parse).map(_.get).toList // WARN: we let it crash if it could not be parsed
  val finalState = steps.foldLeft(State(Coords(0, 0), North, Vector.empty)) {
    case (s@State(Coords(x0, y0), North, _), Left(n)) => s.to(Coords(-n, 0), West)
    case (s@State(Coords(x0, y0), North, _), Right(n)) => s.to(Coords(n, 0), East)

    case (s@State(Coords(x0, y0), East, _), Left(n)) => s.to(Coords(0, n), North)
    case (s@State(Coords(x0, y0), East, _), Right(n)) => s.to(Coords(0, -n), South)

    case (s@State(Coords(x0, y0), South, _), Left(n)) => s.to(Coords(n, 0), East)
    case (s@State(Coords(x0, y0), South, _), Right(n)) => s.to(Coords(-n, 0), West)

    case (s@State(Coords(x0, y0), West, _), Left(n)) => s.to(Coords(0, -n), South)
    case (s@State(Coords(x0, y0), West, _), Right(n)) => s.to(Coords(0, n), North)
  }

  println(s"Final location is ${Math.abs(finalState.pos.x) + Math.abs(finalState.pos.y)} blocks away")

  val positions = finalState.positionHistory
  val occurrences = positions.foldLeft(Map.empty[Coords, Int].withDefaultValue(0)) { case (map, position) =>
      map.updated(position, map(position) + 1)
  }

  val result = positions.find(p => occurrences(p) >= 2)
  result match {
    case Some(Coords(x, y)) => println(s"First visited twice is ${Math.abs(x) + Math.abs(y)} blocks away")
    case None => println("No location visited twice")
  }
}
