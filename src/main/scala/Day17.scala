import java.security.MessageDigest
import scala.collection.mutable

object Day17 extends App {
  val md5 = MessageDigest.getInstance("MD5")

  val input = "njfxhljp"
  val width = 4
  val height = 4

  case class Position(x: Int, y: Int) {
    def move(step: Step): Position = step match {
      case Left => Position(x-1, y)
      case Right => Position(x+1, y)
      case Up => Position(x, y-1)
      case Down => Position(x, y+1)
    }

    def canMove(step: Step): Boolean = step match {
      case Left => x > 1
      case Right => x < width
      case Up => y > 1
      case Down => y < height
    }
  }

  sealed trait Step {
    def toChar: Char
    def statusIndex: Byte
  }
  case object Left extends Step {
    override def toChar: Char = 'L'
    override def statusIndex: Byte = 2
  }
  case object Right extends Step {
    override def toChar: Char = 'R'
    override def statusIndex: Byte = 3
  }
  case object Up extends Step {
    override def toChar: Char = 'U'
    override def statusIndex: Byte = 0
  }
  case object Down extends Step {
    override def toChar: Char = 'D'
    override def statusIndex: Byte = 1
  }

  case class State(position: Position, path: Vector[Step]) {
    def move(by: Step): State = {
      State(position.move(by), path :+ by)
    }
  }

  def estimateCost(from: Position, to: Position): Double = {
    val dx = to.x - from.x
    val dy = to.y - from.y
    dx * dx + dy * dy
  }

  case class SearchStep(state: State, cost: Double) {
    def next(step: Step, goal: Position): SearchStep = {
      val nextState = state.move(step)
      val estimation = nextState.path.length + estimateCost(nextState.position, goal)
      SearchStep(nextState, estimation)
    }
  }

  def hash(initial: String, path: Vector[Step]): Array[Byte] =
    md5.digest((initial + path.map(_.toChar).mkString).getBytes)

  def fromHash(hash: Array[Byte], index: Int): Int = {
    val b = hash(index / 2)
    if (index % 2 == 0) {
      (b & 0xF0) >> 4
    } else {
      b & 0x0F
    }
  }

  def possibleNextSteps(currentPosition: Position, currentHash: Array[Byte]): Set[Step] = {
    Set[Step](Up, Down, Left, Right)
      .filter(currentPosition.canMove)
      .filter(step => fromHash(currentHash, step.statusIndex) > 10)
  }

  implicit val stepOrdering = new Ordering[SearchStep] {
    override def compare(x: SearchStep, y: SearchStep): Int =
      -x.cost.compareTo(y.cost)
  }

  def findShortest(initial: String, from: Position, to: Position): Option[Vector[Step]] = {
    val start = State(from, Vector.empty)
    val startingState = SearchStep(start, estimateCost(from, to))
    val searchSteps: mutable.PriorityQueue[SearchStep] = mutable.PriorityQueue(startingState)
    var result: Option[Vector[Step]] = None

    while (searchSteps.nonEmpty && result.isEmpty) {
      val searchStep = searchSteps.dequeue

      if (searchStep.state.position == to) {
        result = Some(searchStep.state.path)
      } else {
        val currentHash = hash(initial, searchStep.state.path)
        val steps = possibleNextSteps(searchStep.state.position, currentHash)
        for (step <- steps) {
          val nextStep = searchStep.next(step, to)
          searchSteps.enqueue(nextStep)
        }
      }
    }

    result
  }

  def replay(initial: String, from: Position, steps: Vector[Step]): Unit = {
    steps.foldLeft((initial, from)) { case ((current, position), step) =>
      val next = position.move(step)
      val hash = md5.digest(current.getBytes)
      val possibleSteps = possibleNextSteps(position, hash)
      val codes = (0 to 3).map(fromHash(hash, _)).toList
      println(s"$position $step possibilities: $possibleSteps codes: $codes")
      (current+step.toChar, next)
    }
  }

  def findAllPaths(initial: String, from: Position, to: Position): Vector[Vector[Step]] = {
    val start = State(from, Vector.empty)
    val startingState = SearchStep(start, estimateCost(from, to))
    var searchSteps: List[SearchStep] = List(startingState)
    var result: Vector[Vector[Step]] = Vector.empty
    var reportTick: Long = System.currentTimeMillis()

    while (searchSteps.nonEmpty) {
      val searchStep = searchSteps.head
      searchSteps = searchSteps.tail

      val tick = System.currentTimeMillis()
      if ((tick - reportTick) > 5000) {
        println(s"Found paths: ${result.length}; queue length: ${searchSteps.length}; current longest: ${result.map(_.length).max}")
        reportTick = tick
      }

      if (searchStep.state.position == to) {
        result = result :+ searchStep.state.path
      } else {
        val currentHash = hash(initial, searchStep.state.path)
        val steps = possibleNextSteps(searchStep.state.position, currentHash)
        for (step <- steps) {
          val nextStep = searchStep.next(step, to)
          searchSteps = nextStep :: searchSteps
        }
      }
    }

    result
  }

  val shortest = findShortest(input, Position(1, 1), Position(width, height)).map(steps => steps.map(_.toChar).mkString)
  println(s"Shortest: $shortest")

  val allResults = findAllPaths(input, Position(1, 1), Position(width, height))
  println(s"Found ${allResults.length} paths, longest: ${allResults.map(_.length).max}")
}
