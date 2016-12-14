import java.security.MessageDigest

import scala.annotation.tailrec

object Day14 extends App {
  val salt = "ihaygndm"
  val md5 = MessageDigest.getInstance("MD5")

  type Hash = Array[Byte]

  def hash(index: Int): Hash =
    md5.digest((salt + index.toString).getBytes)

  val hexChars = "0123456789abcdef"

  def toAsciiHexString(hash: Hash): Array[Byte] = {
    val result = new Array[Byte](hash.length * 2)
    for (idx <- hash.indices) {
      val a = hash(idx)
      val hi = (a & 0xF0) >> 4
      val lo = a & 0x0F
      result(idx*2) = hexChars(hi).toByte
      result(idx*2+1) = hexChars(lo).toByte
    }
    result
  }

  def stretchedHash(index: Int): Hash = {
    @tailrec
    def rehash(hash: Hash, remaining: Int): Hash = {
      if (remaining == 0) {
        hash
      } else {
        rehash(md5.digest(toAsciiHexString(hash)), remaining - 1)
      }
    }

    rehash(hash(index), 2016)
  }

  def printHash(hash: Hash): String =
    hash.map(_.formatted("%02x")).mkString

  def containsTriplet(hash: Hash): Option[Byte] = {
    (0 until hash.length - 1)
      .find { idx =>
        val a = hash(idx)
        val b = hash(idx + 1)

        val c1 = (a & 0xF0) >> 4
        val c2 = a & 0x0F
        val c3 = (b & 0xF0) >> 4
        val c4 = b & 0x0F

        (c1 == c2 && c1 == c3) || (c2 == c3 && c2 == c4)
      }.map { idx => (hash(idx) & 0x0F).toByte }
  }

  def matches(char: Byte)(hash: Hash): Boolean = {
    val hi = char << 4
    val lo = char
    val mid = hi | lo
    (0 until hash.length - 2).exists { idx =>
      val a = hash(idx)
      val b = hash(idx + 1)
      val c = hash(idx + 2)

      val c1 = (a & 0xF0) >> 4
      val c2 = a & 0x0F
      val c3 = (b & 0xF0) >> 4
      val c4 = b & 0x0F
      val c5 = (c & 0xF0) >> 4
      val c6 = c & 0x0F

      (c1 == char && c2 == char && c3 == char && c4 == char && c5 == char) ||
        (c2 == char && c3 == char && c4 == char && c5 == char && c6 == char)
    }
  }

  def generateHashes(from: Int): Stream[Hash] = {
    Stream.from(from).map(hash)
  }

  def generateStretchedHashes(from: Int): Stream[Hash] = {
    Stream.from(from).map(stretchedHash)
  }

  def printResult(stream: Stream[Hash]): Unit = {
    stream
      .zipWithIndex
      .sliding(1000)
      .map { hashes =>
        val (hash, idx) = hashes.head
        containsTriplet(hash) match {
          case Some(char) => (hash, idx, hashes.tail.map { case (h, _) => h }.exists(matches(char)))
          case None => (hash, idx, false)
        }
      }
      .filter { case (h, idx, valid) => valid }
      .slice(63, 64).foreach { case (_, idx, _) =>
      println(s"64th index is $idx")
    }
  }

  println("Normal hashes:")
  printResult(generateHashes(0))

  println("Stretched hashes:")
  printResult(generateStretchedHashes(0))
}
