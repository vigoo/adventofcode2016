import scala.collection.mutable
import scala.io.Source
import scala.util.hashing.MurmurHash3

object Day22 extends App {

  case class Move(fromX: Int, fromY: Int, toX: Int, toY: Int) {
    override def toString: String = s"<$fromX, $fromY> => <$toX, $toY>"
  }

  case class Node(x: Int, y: Int, total: Int)

  class GridSnapshot(val data: Array[Int]) {
    override def hashCode(): Int =
      MurmurHash3.arrayHash(data)

    override def equals(obj: scala.Any): Boolean =
      data.sameElements(obj.asInstanceOf[GridSnapshot].data)
  }

  trait Grid {
    def width: Int
    def height: Int
    def goalX: Int
    def goalY: Int
    def getInfo(x: Int, y: Int): Node
    def getUsed(x: Int, y: Int): Int
    def allNodes: Seq[Node]
    def move(move: Move): Grid
    def snapshot(): GridSnapshot
  }

  final class BaseGrid(val width: Int, val height: Int, val goalX: Int, val goalY: Int) extends Grid {

    private val nodes: Array[Node] = new Array(width*height)
    private val used: Array[Int] = new Array(width*height)

    override def getInfo(x: Int, y: Int): Node =
      nodes(indexOf(x, y))

    override def getUsed(x: Int, y: Int): Int =
      used(indexOf(x, y))

    def set(x: Int, y: Int, node: Node, used: Int): Unit = {
      nodes(indexOf(x, y)) = node
      this.used(indexOf(x, y)) = used
    }

    override def allNodes: Seq[Node] = nodes

    override def move(move: Move): Grid = {
      val modifiedGrid = new ModifiedGrid(this, snapshot())
      modifiedGrid.move(move)
    }

    override def snapshot(): GridSnapshot = {
      val snapshot = new Array[Int](width*height + 2)
      snapshot(0) = goalX
      snapshot(1) = goalY
      for (idx <- used.indices)
        snapshot(idx + 2) = used(idx)
      new GridSnapshot(snapshot)
    }

    private def indexOf(x: Int, y: Int): Int =
      y*width + x
  }

  final class ModifiedGrid(baseGrid: BaseGrid, override val snapshot: GridSnapshot) extends Grid {
    override def width: Int = baseGrid.width

    override def height: Int = baseGrid.height

    override def goalX: Int = snapshot.data(0)

    override def goalY: Int = snapshot.data(1)

    override def getInfo(x: Int, y: Int): Node =
      baseGrid.getInfo(x, y)

    override def getUsed(x: Int, y: Int): Int =
      snapshot.data(2 + (y*baseGrid.width) + x)

    override def allNodes: Seq[Node] = baseGrid.allNodes

    override def move(move: Move): Grid = {
      val newSnapshot = new Array[Int](width*height + 2)
      val fromIdx = 2 + move.fromY*baseGrid.width + move.fromX
      val toIdx = 2 + move.toY*baseGrid.width + move.toX
      val newGoalX = if (move.fromX == goalX && move.fromY == goalY) move.toX else goalX
      val newGoalY = if (move.fromX == goalX && move.fromY == goalY) move.toY else goalY
      newSnapshot(0) = newGoalX
      newSnapshot(1) = newGoalY

      for (idx <- 2 until snapshot.data.length) {
        newSnapshot(idx) =
          if (idx == fromIdx)
            0
          else if (idx == toIdx)
            snapshot.data(idx) + snapshot.data(fromIdx)
          else
            snapshot.data(idx)
      }

      new ModifiedGrid(baseGrid, new GridSnapshot(newSnapshot))
    }
  }

  case class SearchStep(baseGrid: Grid, steps: Vector[Move], cost: Int) {
    def resultGrid: Grid = if (steps.isEmpty) baseGrid else baseGrid.move(steps.last)

    def next(newBaseGrid: Grid, newGrid: Grid, move: Move): SearchStep = {
      SearchStep(newBaseGrid, steps :+ move, steps.length + 1 + estimateCost(newGrid))
    }
  }

  val dfLine = """\/dev\/grid\/node\-x(\d+)\-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r
  def parseDfOutput(line: String): (Node, Int) = {
    line match {
      case dfLine(x, y, total, used, _, _) =>
        (Node(x.toInt, y.toInt, total.toInt), used.toInt)
    }
  }

  def isViablePair(grid: Grid, ax: Int, ay: Int, bx: Int, by: Int): Boolean = {
    val nodeA = grid.getInfo(ax, ay)
    val usedA = grid.getUsed(ax, ay)
    if (usedA > 0 && (ax != bx || ay != by) && bx >= 0 && bx < grid.width && by >= 0 && by < grid.height) {
      val nodeB = grid.getInfo(bx, by)
      val usedB = grid.getUsed(bx, by)
      (nodeB.total - usedB) > usedA
    } else {
      false
    }
  }

  def canTake(grid: Grid, targetX: Int, targetY: Int, size: Int): Boolean = {
    if (targetX >= 0 && targetX < grid.width &&
        targetY >= 0 && targetY < grid.height) {
      val node = grid.getInfo(targetX, targetY)
      val used = grid.getUsed(targetX, targetY)
      (node.total - used) >= size
    }
    else
      false
  }

  def validMoves(grid: Grid): Seq[Move] = {
    for {
      node <- grid.allNodes
      used = grid.getUsed(node.x, node.y)
      if used > 0
      neighbourX <- Seq(node.x-1, node.x, node.x+1)
      neighbourY <- Seq(node.y-1, node.y, node.y+1)
      if neighbourX == node.x || neighbourY == node.y
      if canTake(grid, neighbourX, neighbourY, used)
    } yield Move(node.x, node.y, neighbourX, neighbourY)
  }

  def isGoalReached(grid: Grid): Boolean = {
    grid.goalX == 0 && grid.goalY == 0
  }

  def closestViablePairDistance(grid: Grid, x: Int, y: Int): Int = {
    (for {
      y2 <- 0 until grid.height
      x2 <- 0 until grid.width
      if isViablePair(grid, x, y, x2, y2)
    } yield Math.abs(x2-x) + Math.abs(y2-y)).min
  }

  def estimateCost(grid: Grid): Int = {
    val size = grid.getUsed(grid.goalX, grid.goalY)
    val count1 =
      (0 until grid.goalX).map { x =>
        val y = grid.goalY
        if (grid.getInfo(x, y).total - grid.getUsed(x, y) < size) closestViablePairDistance(grid, x, y) else 0
      }.sum +
      (0 until grid.goalY).map(y => if (grid.getInfo(0, y).total - grid.getUsed(0, y) < size) closestViablePairDistance(grid, 0, y) else 0).sum
    val count2 =
      (0 until grid.goalY).map { y =>
        val x = grid.goalX
        if (grid.getInfo(x, y).total - grid.getUsed(x, y) < size) closestViablePairDistance(grid, x, y) else 0
      }.sum +
      (0 until grid.goalX).map(x => if (grid.getInfo(x, 0).total - grid.getUsed(x, 0) < size) closestViablePairDistance(grid, x, 0) else 0).sum
    grid.goalX + grid.goalY + Math.min(count1, count2)
  }

  def findShortest(initial: Grid): Option[Vector[Move]] = {
    implicit val ordering = Ordering.by[SearchStep, Int](-_.cost)

    val searchSteps: mutable.PriorityQueue[SearchStep] = mutable.PriorityQueue[SearchStep](SearchStep(initial, Vector.empty, estimateCost(initial)))
    val visited: mutable.Set[GridSnapshot] = mutable.Set(searchSteps.map(_.resultGrid.snapshot()).toSeq : _*)
    var result: Option[Vector[Move]] = None
    var reportTick: Long = System.currentTimeMillis()

    while (searchSteps.nonEmpty && result.isEmpty) {
      val searchStep = searchSteps.dequeue
      val currentGrid = searchStep.resultGrid

      val tick = System.currentTimeMillis()
      if ((tick - reportTick) > 2000) {
        println(s"Current step count: ${searchStep.steps.length}; estimation: ${searchStep.cost} queue length: ${searchSteps.length}; visited states: ${visited.size}")
        println(s"Currently status achieved by ${searchStep.steps.lastOption}")
        dumpGrid(currentGrid)

        reportTick = tick
      }

      if (isGoalReached(currentGrid)) {
        result = Some(searchStep.steps)
      } else {
        val moves = validMoves(currentGrid)
        for (nextMove <- moves) {
          val resultGrid = currentGrid.move(nextMove)
          val snapshot = resultGrid.snapshot()
          if (!visited.contains(snapshot)) {
            val nextStep = searchStep.next(currentGrid, resultGrid, nextMove)
            searchSteps.enqueue(nextStep)
            visited.add(snapshot)
          }
        }
      }
    }

    result
  }

  def dumpGrid(grid: Grid): Unit = {
    for (y <- 0 until grid.height) {
      for (x <- 0 until grid.width) {
        val info = grid.getInfo(x, y)
        val used = grid.getUsed(x, y)

        print(s"${used.formatted("%3d")}/${info.total.formatted("%3d")} ")
      }
      println()
    }
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

  val gridWidth = nodes.map { case (node, _) => node.x }.max + 1
  val gridHeight = nodes.map { case (node, _) => node.y }.max + 1
  val grid = new BaseGrid(gridWidth, gridHeight, gridWidth-1, 0)
  nodes.foreach { case (node, used) =>
    grid.set(node.x, node.y, node, used)
  }

  println(s"Grid is $gridWidth x $gridHeight")

  val possiblePairs =
    for {
      (node, _) <- nodes
      (otherNode, _) <- nodes
    } yield (node.x, node.y, otherNode.x, otherNode.y)
  val viablePairs = possiblePairs.filter { case (ax, ay, bx, by) => isViablePair(grid, ax, ay, bx, by) }
  println(s"Number of viable pairs: ${viablePairs.length}")

  dumpGrid(grid)

  findShortest(grid) match {
    case Some(steps) =>
      println(s"The shortest path takes ${steps.length} steps")
    case None =>
      println("No result")
  }
}
