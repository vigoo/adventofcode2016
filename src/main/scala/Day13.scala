import com.twitter.util.LruMap

import scala.collection.mutable

object Day13 extends App {
  val favouriteNumber = 1362
  val targetX = 31
  val targetY = 39

  sealed trait Node
  case object OpenSpace extends Node
  case object Wall extends Node

  sealed trait Step {
    def dx: Int
    def dy: Int
  }
  case object Left extends Step { val dx: Int = -1; val dy: Int = 0 }
  case object Right extends Step { val dx: Int = 1; val dy: Int = 0 }
  case object Up extends Step { val dx: Int = 0; val dy: Int = -1 }
  case object Down extends Step { val dx: Int = 0; val dy: Int = 1 }

  val possibleSteps: Set[Step] = Set(Left, Right, Up, Down)

  case class State(x: Int, y: Int) {
    def move(step: Step): State =
      State(x + step.dx, y + step.dy)
  }

  case class SearchStep(state: State, history: List[State], cost: Double) {
    def stepCount: Int = history.size

    def all: List[State] = state :: history

    def next(step: Step, goal: State): SearchStep = {
      val nextState = state.move(step)
      val estimation = cost + estimateCost(nextState, goal)
      SearchStep(nextState, state :: history, estimation)
    }
  }

  def calculateNode(x: Int, y: Int): Node = {
    require(x >= 0)
    require(y >= 0)

    val n = (x*x + 3*x + 2*x*y + y + y*y) + favouriteNumber
    val (bitCount, _) = (1 to 32).foldLeft((0, n)) { case ((count, current), _) =>
      (count + current & 1, current >> 1)
    }

    if (bitCount % 2 == 0) OpenSpace else Wall
  }

  object Memoized {
    private val cache = new LruMap[(Int, Int), Node](10000000)

    def apply(x: Int, y: Int): Node = {
      cache.getOrElseUpdate((x, y), calculateNode(x, y))
    }
  }

  def graphicalRepresentation(node: Node): Char = {
    node match {
      case OpenSpace => '.'
      case Wall => '#'
    }
  }

  def estimateCost(state: State, goal: State): Double = {
    val dx = goal.x - state.x
    val dy = goal.y - state.y
    dx * dx + dy * dy
  }

  implicit val stepOrdering = new Ordering[SearchStep] {
    override def compare(x: SearchStep, y: SearchStep): Int =
      -x.cost.compareTo(y.cost)
  }

  def validSteps(state: State): Set[Step] = {
    possibleSteps.filter { step =>
      val nextState = state.move(step)
      nextState.x >= 0 && nextState.y >= 0 && Memoized(nextState.x, nextState.y) == OpenSpace
    }
  }

  def printResult(finalStep: SearchStep): Unit = {
    val visited = finalStep.all.toSet
    val maxX = visited.map(_.x).max
    val maxY = visited.map(_.y).max

    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        if (visited.contains(State(x, y))) {
          if (Memoized(x, y) == Wall) {
            print('X')
          } else {
            print('O')
          }
        } else {
          print(graphicalRepresentation(Memoized(x, y)))
        }
      }
      println()
    }
  }

  def findPath(startX: Int, startY: Int, goalX: Int, goalY: Int): Option[Int] = {
    val start = State(startX, startY)
    val goal = State(goalX, goalY)
    val startingState = SearchStep(start, List.empty, estimateCost(start, goal))
    val searchSteps: mutable.PriorityQueue[SearchStep] = mutable.PriorityQueue(startingState)
    val visited: mutable.Set[State] = mutable.Set.empty
    var result: Option[Int] = None

    while (searchSteps.nonEmpty && result.isEmpty) {
      val searchStep = searchSteps.dequeue

      if (searchStep.state == goal) {
        printResult(searchStep)
        result = Some(searchStep.stepCount)
      } else {
        val steps = validSteps(searchStep.state)
        for (step <- steps) {
          val nextStep = searchStep.next(step, goal)
          if (!visited.contains(nextStep.state)) {
            searchSteps.enqueue(nextStep)
          }
        }
      }
      visited += searchStep.state
    }

    result
  }

  def countReachable(startX: Int, startY: Int, maxDepth: Int): Int = {
    val start = State(startX, startY)
    val queue = mutable.Queue[SearchStep](SearchStep(start, List.empty, 0))
    val visited: mutable.Set[State] = mutable.Set.empty

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current.stepCount <= maxDepth) {
        val steps = validSteps(current.state)
        for (step <- steps) {
          val nextStep = current.next(step, start)
          if (!visited.contains(nextStep.state)) {
            queue.enqueue(nextStep)
          }
        }
        visited += current.state
      }
    }

    visited.size
  }

  val result1 = findPath(1, 1, targetX, targetY)
  println(s"First result is $result1")

  val result2 = countReachable(1, 1, 50)
  println(s"Second result is $result2")
}
