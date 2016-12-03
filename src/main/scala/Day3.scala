import scala.io.Source

object Day3 extends App {

  case class Item(a: Int, b: Int, c: Int) {
    def canBeTriangle: Boolean =
      a + b > c &&
      a + c > b &&
      b + c > a
  }

  def parseLine(line: String): Item = {
    val parts = line.split(' ').filter(_.length > 0)
    assert(parts.length == 3)
    Item(parts(0).toInt, parts(1).toInt, parts(2).toInt)
  }

  def transposeItems(items: List[Item]): List[Item] = {
    require(items.length == 3)

    items match {
      case List(x, y, z) =>
        List(
          Item(x.a, y.a, z.a),
          Item(x.b, y.b, z.b),
          Item(x.c, y.c, z.c)
        )
    }
  }

  val input = Source.fromResource("day3.txt")
  val items = input.getLines.map(parseLine).toList

  val horizontalTriangles = items.filter(_.canBeTriangle)
  println(s"Number of possible triangles in horizontal interpretation: ${horizontalTriangles.size}")

  val verticalItems = items.grouped(3).map(transposeItems).flatten
  val verticalTriangles = verticalItems.filter(_.canBeTriangle)
  println(s"Number of possible triangles in vertical interpretation: ${verticalTriangles.size}")
}