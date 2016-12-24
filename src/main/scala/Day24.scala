import scala.collection.mutable
import scala.io.Source

object Day24 extends App {

  sealed trait Tile

  case object Free extends Tile

  case object Wall extends Tile

  case class Poi(id: Int) extends Tile

  case class Location(x: Int, y: Int)

  type TileMap = Vector[Vector[Tile]]

  def parseTile(ch: Char): Tile = {
    ch match {
      case '#' => Wall
      case '.' => Free
      case n => Poi(n.toString.toInt)
    }
  }

  def collectPois(map: TileMap): Map[Int, Location] = {
    var result = Map.empty[Int, Location]
    for (y <- map.indices) {
      for (x <- map(y).indices) {
        map(y)(x) match {
          case Poi(id) =>
            result = result.updated(id, Location(x, y))
          case _ =>
        }
      }
    }
    result
  }

  case class Step(dx: Int, dy: Int)

  case class State(location: Location) {
    def move(step: Step): State =
      State(Location(location.x + step.dx, location.y + step.dy))
  }

  case class SearchStep(state: State, history: List[State]) {
    def stepCount: Int = history.size

    def all: List[State] = state :: history

    def next(step: Step): SearchStep = {
      val nextState = state.move(step)
      SearchStep(nextState, state :: history)
    }
  }

  val possibleSteps = Set(Step(-1, 0), Step(1, 0), Step(0, -1), Step(0, 1))

  def validSteps(map: TileMap, state: State): Set[Step] = {
    possibleSteps.filter { step =>
      val nextState = state.move(step)
      nextState.location.x >= 0 && nextState.location.y >= 0 && nextState.location.x < map(0).length && nextState.location.y < map.length &&
      map(nextState.location.y)(nextState.location.x) != Wall
    }
  }

  def collectDistancesToPoisFrom(from: Location): Map[Int, Int] = {

    println(s"Starting from $from")

    val start = State(from)
    val queue = mutable.Queue[SearchStep](SearchStep(start, List.empty))
    val visited: mutable.Set[State] = mutable.Set(State(from))
    val result: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]
    var reportTick: Long = System.currentTimeMillis()

    while (queue.nonEmpty && result.size < 6) {
      val current = queue.dequeue()

      val tick = System.currentTimeMillis()
      if ((tick - reportTick) > 2000) {
        println(s"Current step count: ${current.all.length}; queue length: ${queue.length}; visited states: ${visited.size}; result: ${result}")
        reportTick = tick
      }

      map(current.state.location.y)(current.state.location.x) match {
        case Poi(id) =>
          result += (id -> current.history.length)
        case _ =>
      }

      val steps = validSteps(map, current.state)
      for (step <- steps) {
        val nextStep = current.next(step)
        if (!visited.contains(nextStep.state)) {
          queue.enqueue(nextStep)
          visited += nextStep.state
        }
      }
    }

    result.toList.toMap
  }

  def findShortestPath(distances: Map[Int, Map[Int, Int]], withReturn: Boolean): Int = {

    def cost(current: Int, permutation: List[Int], total: Int): Int = {
      permutation match {
        case id :: rest =>
          distances(current).get(id) match {
            case Some(dist) => cost(id, rest, total + dist)
            case None => cost(id, rest, total + 100000)
          }
        case Nil => total
      }
    }

    val permutations =
      distances.keys.toList.permutations
        .filter(_.head == 0)
        .map(permutation => if (withReturn) permutation ++ List(0) else permutation)

    val (bestPermutation, bestCost) = permutations.map(permutation => (permutation, cost(0, permutation.tail, 0))).minBy { case (_, cost) => cost }
    println(s"Best permutation: $bestPermutation")
    bestCost
  }

  val map = Source.fromResource("day24.txt").getLines.toVector.map(_.toVector.map(parseTile))

//  val example = """###########
//                  |#0.1.....2#
//                  |#.#######.#
//                  |#4.......3#
//                  |###########""".stripMargin
//  val map = example.lines.toVector.map(_.toVector.map(parseTile))

  val poiLocations = collectPois(map)

  println(poiLocations)

  val distances = poiLocations.map { case (id, location) => id -> collectDistancesToPoisFrom(location) }
  println(distances)

  val result1 = findShortestPath(distances, withReturn = false)
  println(s"Shortest path is $result1 steps")

  val result2 = findShortestPath(distances, withReturn = true)
  println(s"Shortest path with returning to 0 is $result2 steps")
}
