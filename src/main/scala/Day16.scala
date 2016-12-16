import scala.annotation.tailrec

object Day16 extends App {

  case class Task(input: Vector[Int], desiredLength: Int)

  def reverseInverse(in: Vector[Int]): Vector[Int] = {
    in.reverse.map(b => if (b == 0) 1 else 0)
  }

  def grow(in: Vector[Int]): Vector[Int] = {
    (in :+ 0) ++ reverseInverse(in)
  }

  @tailrec
  def growUntil(in: Vector[Int], desiredLength: Int): Vector[Int] = {
    if (in.length < desiredLength) {
      growUntil(grow(in), desiredLength)
    } else {
      in.take(desiredLength)
    }
  }

  def checksum(of: Vector[Int]): Vector[Int] = {
    (
      for {
        idx <- 0 until (of.length / 2)
        a = of(idx * 2)
        b = of(idx * 2 + 1)
      } yield if (a == b) 1 else 0
    ).toVector
  }

  @tailrec
  def finalChecksum(of: Vector[Int]): Vector[Int] = {
    val current = checksum(of)
    if (current.length % 2 == 0) {
      finalChecksum(current)
    } else {
      current
    }
  }

  def solve(task: Task): Unit = {
    val data = growUntil(task.input, task.desiredLength)
    val checksum = finalChecksum(data)
    println(checksum.mkString)
  }

  val example = Task(Vector(1, 0, 0, 0, 0), 20)
  val input1 = Task(Vector(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0), 272)
  val input2 = Task(input1.input, 35651584)

  solve(example)
  solve(input1)
  solve(input2)
}
