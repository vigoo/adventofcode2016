import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.io.Source

object Day20 extends App {

  case class IP(address: Long) extends AnyVal {
    def <(other: IP): Boolean = address < other.address

    def <=(other: IP): Boolean = address <= other.address

    def next(): IP = IP(address + 1)
  }

  case class Interval(from: IP, to: IP)

  implicit val intervalOrdering: Ordering[Interval] = Ordering.by[Interval, Long](_.from.address)

  def parseInterval(line: String): Interval = {
    val parts = line.split('-')
    Interval(IP(parts(0).toLong), IP(parts(1).toLong))
  }

  def max(a: IP, b: IP): IP =
    if (a < b) b else a

  @tailrec
  def findFirstNotBlocked(start: IP, currentUpperLimit: IP, blockedIntervals: Seq[Interval]): IP = {
    blockedIntervals match {
      case Interval(from, to) +: remainingIntervals =>
        if (start < from) {
          start
        } else {
          val nextUpperLimit = max(to, currentUpperLimit)
          val nextStart = max(nextUpperLimit, start)
          findFirstNotBlocked(nextStart.next(), nextUpperLimit, remainingIntervals)
        }
      case _ =>
        start
    }
  }


  val intervals = Source.fromResource("day20.txt").getLines.toSeq.map(parseInterval)
  val sortedIntervals = SortedSet[Interval](intervals: _*)
  val firstNonBlocked = findFirstNotBlocked(IP(0), IP(-1), sortedIntervals.toSeq)
  println(s"The first non-blocked IP is $firstNonBlocked")
}