import scala.collection.mutable
import scala.io.Source
import scala.util.hashing.MurmurHash3

object Day22 extends App {


  def asT(size: Long): String = {
    (size / (1024L * 1024L * 1024L * 1024L)).toString + "T"
  }

  case class GridPosition(x: Int, y: Int) {
    override def toString: String = s"<$x, $y>"
  }

  case class Move(from: GridPosition, to: GridPosition) {
    override def toString: String = s"$from => $to"
  }

  case class Node(position: GridPosition, total: Long, used: Long) {
    require(used <= total)

    def avail: Long = total - used

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
      s"/dev/grid/node-x${position.x}-y${position.y}\t${asT(total)}\t${asT(used)}\t${asT(avail)}\t${((used.toDouble/total.toDouble)*100.0).formatted("%3.0f")}%"
    }
  }

  class GridSnapshot(private val data: Array[Long]) {
    override def hashCode(): Int =
      MurmurHash3.arrayHash(data)

    override def equals(obj: scala.Any): Boolean =
      data.sameElements(obj.asInstanceOf[GridSnapshot].data)
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

    def snapshot(): GridSnapshot = {
      val snapshot = new Array[Long](width*height + 1)
      snapshot(0) = goalLocation.x << 32 | goalLocation.y
      for (idx <- nodes.indices)
        snapshot(idx + 1) = nodes(idx).used
      new GridSnapshot(snapshot)
    }

    private def indexOf(position: GridPosition): Int =
      position.y*width + position.x
  }

  case class SearchStep(resultGrid: Grid, steps: Vector[Move], cost: Int) {
    def next(newGrid: Grid, move: Move): SearchStep = {
      SearchStep(newGrid, steps :+ move, steps.length + 1 + estimateCost(newGrid))
    }
  }

  val dfLine = """\/dev\/grid\/node\-x(\d+)\-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r
  def parseDfOutput(line: String): Node = {
    line match {
      case dfLine(x, y, total, used, _, _) =>
        val position = GridPosition(x.toInt, y.toInt)
        Node(position, total.toLong * 1024 * 1024 * 1024 * 1024, used.toLong * 1024 * 1024 * 1024 * 1024)
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

  def canTake(grid: Grid, target: GridPosition, size: Long): Boolean = {
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
    implicit val ordering = Ordering.by[SearchStep, Int](-_.cost)

    val searchSteps: mutable.PriorityQueue[SearchStep] = mutable.PriorityQueue[SearchStep](SearchStep(initial, Vector.empty, estimateCost(initial)))
    val visited: mutable.Set[GridSnapshot] = mutable.Set(searchSteps.map(_.resultGrid.snapshot()).toSeq : _*)
    var result: Option[Vector[Move]] = None
    var reportTick: Long = System.currentTimeMillis()

    while (searchSteps.nonEmpty && result.isEmpty) {
      val searchStep = searchSteps.dequeue

      val tick = System.currentTimeMillis()
      if ((tick - reportTick) > 2000) {
        println(s"Current step count: ${searchStep.steps.length}; estimation: ${searchStep.cost} queue length: ${searchSteps.length}; visited states: ${visited.size}")
        reportTick = tick
      }

      if (isGoalReached(searchStep.resultGrid)) {
        result = Some(searchStep.steps)
      } else {
        val moves = validMoves(searchStep.resultGrid)
        for (nextMove <- moves) {
          val resultGrid = searchStep.resultGrid.move(nextMove)
          val snapshot = resultGrid.snapshot()
          if (!visited.contains(snapshot)) {
            val nextStep = searchStep.next(resultGrid, nextMove)
            searchSteps.enqueue(nextStep)
            visited.add(snapshot)
          }
        }
      }
    }

    result
  }

  val nodes = Source.fromResource("day22.txt").getLines.drop(2).map(parseDfOutput).toList
//  val nodes =
//    """/dev/grid/node-x0-y0   10T    8T     2T   80%
//      |/dev/grid/node-x0-y1   11T    6T     5T   54%
//      |/dev/grid/node-x0-y2   32T   28T     4T   87%
//      |/dev/grid/node-x1-y0    9T    7T     2T   77%
//      |/dev/grid/node-x1-y1    8T    0T     8T    0%
//      |/dev/grid/node-x1-y2   11T    7T     4T   63%
//      |/dev/grid/node-x2-y0   10T    6T     4T   60%
//      |/dev/grid/node-x2-y1    9T    8T     1T   88%
//      |/dev/grid/node-x2-y2    9T    6T     3T   66%""".stripMargin.lines.map(parseDfOutput).toList

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
