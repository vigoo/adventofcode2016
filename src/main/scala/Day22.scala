import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.hashing.MurmurHash3

object Day22 extends App {

  case class GridPosition(x: Int, y: Int) {
    override def toString: String = s"<$x, $y>"
  }

  case class Move(from: GridPosition, to: GridPosition) {
    override def toString: String = s"$from => $to"
  }

  case class Node(position: GridPosition, total: Int, used: Int) {
    require(used <= total)

    def avail: Int = total - used

    def neighbours: Seq[GridPosition] = {
      Seq(
        GridPosition(position.x - 1, position.y),
        GridPosition(position.x + 1, position.y),
        GridPosition(position.x, position.y - 1),
        GridPosition(position.x, position.y + 1)
      )
    }

    def isEmpty: Boolean = used == 0
    def nonEmpty: Boolean = used > 0

    def emptied: Node =
      this.copy(used = 0)

    def add(other: Node): Node =
      this.copy(used = used + other.used)

    override def toString: String = {
      s"/dev/grid/node-x${position.x}-y${position.y}\t$total\t$used\t$avail\t${((used.toDouble/total.toDouble)*100.0).formatted("%3.0f")}%"
    }
  }

  class Grid(width: Int, height: Int, val goalLocation: GridPosition) {

    private val nodes: Array[Node] = new Array(width*height)

    def apply(position: GridPosition): Node =
      nodes(indexOf(position))

    def update(position: GridPosition, node: Node): Unit =
      nodes(indexOf(position)) = node

    def get(position: GridPosition): Option[Node] =
      if (position.x >= 0 && position.x < width &&
        position.y >= 0 && position.y < height)
        Some(nodes(indexOf(position)))
      else
        None

    def allNodes: Seq[Node] = nodes

    def move(move: Move): Grid = {
      val fromIdx = indexOf(move.from)
      val toIdx = indexOf(move.to)
      val newGoalLocation = if (move.from == goalLocation) move.to else goalLocation
      val result = new Grid(width, height, newGoalLocation)

      for (idx <- nodes.indices) {
        result.nodes(idx) =
          if (idx == fromIdx)
            nodes(idx).emptied
          else if (idx == toIdx)
            nodes(idx).add(nodes(fromIdx))
          else
            nodes(idx)
      }

      result
    }

    def dump(): Unit = {
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val node = this(GridPosition(x, y))

          print(s"${node.used.formatted("%3d")}/${node.total.formatted("%3d")} ")
        }
        println()
      }
    }

    private def indexOf(position: GridPosition): Int =
      position.y*width + position.x
  }

  val dfLine = """\/dev\/grid\/node\-x(\d+)\-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r
  def parseDfOutput(line: String): Node = {
    line match {
      case dfLine(x, y, total, used, _, _) =>
        val position = GridPosition(x.toInt, y.toInt)
        Node(position, total.toInt, used.toInt)
    }
  }

  def isViablePair(grid: Grid, a: GridPosition, b: GridPosition): Boolean = {
    val nodeA = grid(a)
    if (nodeA.nonEmpty && a != b) {
      grid.get(b) match {
        case Some(nodeB) => nodeB.avail > nodeA.used
        case None => false
      }
    } else {
      false
    }
  }

  def canTake(grid: Grid, target: GridPosition, size: Int): Boolean = {
    grid.get(target) match {
      case Some(node) =>
        node.avail >= size
      case None => false
    }
  }

  def validMoves(grid: Grid): Seq[Move] = {
    for {
      node <- grid.allNodes
      if node.nonEmpty
      neighbour <- node.neighbours
      if canTake(grid, neighbour, node.used)
    } yield Move(node.position, neighbour)
  }

  def isGoalReached(grid: Grid): Boolean = {
    grid.goalLocation == GridPosition(0, 0)
  }

  def estimateCost(grid: Grid): Int = {
    grid.goalLocation.x + grid.goalLocation.y
  }

  def findShortest(initial: Grid): Option[Vector[Move]] = {

    def nodeCloserTo(target: GridPosition)(a: Node, b: Node): Boolean =
      closerTo(target)(a.position, b.position)

    def closerTo(target: GridPosition)(a: GridPosition, b: GridPosition): Boolean = {
      val dxa = Math.abs(a.x - target.x)
      val dya = Math.abs(a.y - target.y)
      val dxb = Math.abs(b.x - target.x)
      val dyb = Math.abs(b.y - target.y)

      (dxa + dya) < (dxb + dyb)
    }

    def tryPath(state: Grid, from: Node, to: Node): Option[Move] = {
      val validMoves = from.neighbours
          .filter(p => p != state.goalLocation)
          .filter(p => canTake(state, from.position, state(p).used)).sortWith(closerTo(to.position))
      validMoves.headOption match {
        case Some(move) => Some(Move(move, from.position))
        case None => None
      }
    }

    @tailrec
    def tryPaths(state: Grid, to: Node, orderedTargets: Seq[Node]): Option[Move] = {
      orderedTargets match {
        case target +: rest =>
          tryPath(state, target, to) match {
            case Some(result) => Some(result)
            case None => tryPaths(state, to, rest)
          }
        case _ =>
          None
      }
    }

    def step(state: Grid): Option[Move] = {
      if (isGoalReached(state))
        None
      else {
        val goalNode = state(state.goalLocation)
        val toLeft = GridPosition(state.goalLocation.x - 1, state.goalLocation.y)
        val nodeToLeft = state(toLeft)
        if (nodeToLeft.avail >= goalNode.used) {
          Some(Move(state.goalLocation, toLeft))
        } else {
          val possibleTargets =
            state.allNodes.filter(otherNode => isViablePair(state, nodeToLeft.position, otherNode.position))
          val immediateTargets = possibleTargets.filter(target => nodeToLeft.neighbours.contains(target.position))
          immediateTargets.headOption match {
            case Some(immediateTarget) =>
              Some(Move(toLeft, immediateTarget.position))
            case None =>
              val orderedTargets = possibleTargets.sortWith(nodeCloserTo(toLeft))
              tryPaths(state, nodeToLeft, orderedTargets) match {
                case Some(result) => Some(result)
                case None =>
                  val possiblePairs =
                    for {
                      node <- state.allNodes
                      otherNode <- state.allNodes
                    } yield (node.position, otherNode.position)
                  val viablePairs = possiblePairs.filter { case (a, b) => isViablePair(state, a, b) }
                  val sortedViablePairs = viablePairs.sortWith((a, b) => closerTo(toLeft)(a._1, b._1))
                  sortedViablePairs.headOption match {
                    case Some((from, to)) =>
                      tryPaths(state, state(from), Seq(state(to)))
                    case None =>
                      None
                  }

              }
          }
        }
      }
    }

    @tailrec
    def collectSteps(state: Grid, steps: Vector[Move]): (Grid, Vector[Move]) = {
      state.dump()
      val nextStep = step(state)
      println(s"STEP: $nextStep")
      nextStep match {
        case Some(move) =>
          collectSteps(state.move(move), steps :+ move)
        case None =>
          (state, steps)
      }
    }

    val (finalState, steps) = collectSteps(initial, Vector.empty)
    if (isGoalReached(finalState)) {
      Some(steps)
    } else {
      None
    }
  }

  val nodes = Source.fromResource("day22.txt").getLines.drop(2).map(parseDfOutput).toList
//    val nodes =
//      """/dev/grid/node-x0-y0   10T    8T     2T   80%
//        |/dev/grid/node-x0-y1   11T    6T     5T   54%
//        |/dev/grid/node-x0-y2   32T   28T     4T   87%
//        |/dev/grid/node-x1-y0    9T    7T     2T   77%
//        |/dev/grid/node-x1-y1    8T    0T     8T    0%
//        |/dev/grid/node-x1-y2   11T    7T     4T   63%
//        |/dev/grid/node-x2-y0   10T    6T     4T   60%
//        |/dev/grid/node-x2-y1    9T    8T     1T   88%
//        |/dev/grid/node-x2-y2    9T    6T     3T   66%""".stripMargin.lines.map(parseDfOutput).toList

  val gridWidth = nodes.map(_.position.x).max + 1
  val gridHeight = nodes.map(_.position.y).max + 1
  val grid = new Grid(gridWidth, gridHeight, GridPosition(gridWidth-1, 0))
  nodes.foreach { node =>
    grid(node.position) = node
  }

  println(s"Grid is $gridWidth x $gridHeight")

  val possiblePairs =
    for {
      node <- nodes
      otherNode <- nodes
    } yield (node.position, otherNode.position)
  val viablePairs = possiblePairs.filter { case (a, b) => isViablePair(grid, a, b) }
  println(s"Number of viable pairs: ${viablePairs.length}")

  findShortest(grid) match {
    case Some(steps) =>
      println(s"The shortest path takes ${steps.length} steps")
    case None =>
      println("No result")
  }
}