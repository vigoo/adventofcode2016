import scala.collection.SortedSet
import scala.io.Source

object Day20 extends App {

  case class IP(address: Long) extends AnyVal {
    def <(other: IP): Boolean = address < other.address

    def <=(other: IP): Boolean = address <= other.address

    def next(): IP = IP(address + 1)
  }

  case class Interval(from: IP, to: IP) {
    require(from <= to)
  }

  implicit val intervalOrdering: Ordering[Interval] = Ordering.by[Interval, Long](_.from.address)

  def parseInterval(line: String): Interval = {
    val parts = line.split('-')
    Interval(IP(parts(0).toLong), IP(parts(1).toLong))
  }

  def max(a: IP, b: IP): IP =
    if (a < b) b else a

  def findAllNonBlocked(start: IP, currentUpperLimit: IP, last: IP, blockedIntervals: Seq[Interval]): Stream[IP] = {
    blockedIntervals match {
      case Interval(from, to) +: remainingIntervals =>
        val nextUpperLimit = max(to, currentUpperLimit)
        val nextStart = max(nextUpperLimit.next(), start)


        if (start < from) {
          Stream.range(start.address, from.address).map(IP) ++ findAllNonBlocked(nextStart.next(), nextUpperLimit, last, remainingIntervals)
        }
        else {
          findAllNonBlocked(nextStart, nextUpperLimit, last, remainingIntervals)
        }
      case _ =>
        Stream.range(currentUpperLimit.next().address, last.next().address).map(IP)
    }
  }


  val intervals = Source.fromResource("day20.txt").getLines.toList.map(parseInterval)
  val sortedIntervals = SortedSet[Interval](intervals: _*)
  val allNonBlocked = findAllNonBlocked(IP(0L), IP(-1L), IP(4294967295L), sortedIntervals.toSeq)

  val firstNonBlocked = allNonBlocked.head
  println(s"The first non-blocked IP is $firstNonBlocked")

  val nonBlockedCount = allNonBlocked.length
  println(s"Number of non-blocked IPs is $nonBlockedCount")

  allNonBlocked.foreach(println)
}