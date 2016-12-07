import scala.io.Source

object Day7 extends App {

  case class Abba(c0: Char, c1: Char, c2: Char, c3: Char) {
    def isValid: Boolean = c0 != c1 && c0 == c3 && c1 == c2
  }

  case class Aba(c0: Char, c1: Char, c2: Char) {
    def isValid: Boolean = c0 != c1 && c0 == c2
    def bab: String = s"$c1$c0$c1"
  }

  def hasAbba(s: String): Boolean = {
    s.sliding(4).exists(s4 => Abba(s4(0), s4(1), s4(2), s4(3)).isValid)
  }

  def getAbas(s: String): Set[Aba] = {
    s.sliding(3).map(s3 => Aba(s3(0), s3(1), s3(2))).filter(_.isValid).toSet
  }

  case class IP(ip: String) {
    private val parts = ip.split(Array('[', ']')).toVector
    private val paddedParts = if (parts.length % 2 == 1) parts :+ "" else parts

    private val normalSequences = paddedParts.grouped(2).map { case Vector(normal, _) => normal }.toList
    private val hypernetSequences = paddedParts.grouped(2).map { case Vector(_, hypernet) => hypernet }.toList

    def supportsTLS: Boolean = {
      normalSequences.exists(hasAbba) && hypernetSequences.forall(s => !hasAbba(s))
    }

    def supportsSSL: Boolean = {
      val abas = normalSequences.foldLeft(Set.empty[Aba]) { case (result, s) => result.union(getAbas(s)) }
      abas.exists(aba => hypernetSequences.exists(_.contains(aba.bab)))
    }
  }

  val input = Source.fromResource("day7.txt")
  val ips = input.getLines().toList.map(IP)

  val tlsCount = ips.count(_.supportsTLS)
  println(s"Number of IPs supporting TLS: $tlsCount")

  val sslCount = ips.count(_.supportsSSL)
  println(s"Number of IPs supporting SSL: $sslCount")
}
