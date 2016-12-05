import java.security.MessageDigest

import scala.annotation.tailrec

object Day5 extends App {
  val input = "reyedfim"
  val md5 = MessageDigest.getInstance("MD5")

  def genHash(n: Long): Array[Byte] = {
    md5.digest((input + n.toString).getBytes())
  }

  def indicatesPasswordCharacter(hash: Array[Byte]): Boolean = {
    hash(0) == 0 &&
      hash(1) == 0 &&
      hash(2) >= 0 &&
      hash(2) < 16
  }

  def getPasswordChar1(hash: Array[Byte]): Char =
    hash(2).formatted("%02x")(1)

  def getPasswordChar2(hash: Array[Byte]): Char =
    hash(3).formatted("%02x")(0)

  def getTargetPosition(hash: Array[Byte]): Int =
    hash(2) % 16

  def toHexString(hash: Array[Byte]): String =
    hash.map(_.formatted("%02x")).mkString

  case class FindResult(passwordChar: Char, position: Int, n: Long, hash: String)

  @tailrec
  def findNext1(pos: Int, n: Long): FindResult = {
    val hash = genHash(n)
    if (indicatesPasswordCharacter(hash)) {
      FindResult(getPasswordChar1(hash), pos, n, toHexString(hash))
    } else {
      findNext1(pos, n + 1)
    }
  }

  @tailrec
  def findNext2(n: Long, alreadyFound: Set[Int]): FindResult = {
    val hash = genHash(n)
    if (indicatesPasswordCharacter(hash)) {
      val position = getTargetPosition(hash)
      if (position >= 0 && position < 8 && !alreadyFound.contains(position)) {
        FindResult(getPasswordChar2(hash), position, n, toHexString(hash))
      } else {
        findNext2(n + 1, alreadyFound)
      }
    } else {
      findNext2(n + 1, alreadyFound)
    }
  }

  def findPassword(findFun: (Int, Long, Set[Int]) => FindResult): String = {
    val (_, result, _) = (0 to 7).foldLeft((0L, new StringBuilder("?" * 8), Set.empty[Int])) {
      case ((n, builder, alreadyFound), idx) =>
        val result = findFun(idx, n, alreadyFound)
        println(s"$idx: ${result.passwordChar} @ ${result.position} (hash was: ${result.hash})")
        builder.update(result.position, result.passwordChar)
        (result.n + 1, builder, alreadyFound + result.position)
    }
    result.toString()
  }

  val result1 = findPassword { case (idx, n, _) => findNext1(idx, n) }
  println(s"First door code is $result1")

  val result2 = findPassword { case (_, n, alreadyFound) => findNext2(n, alreadyFound) }
  println(s"Second door code is $result2")
}
