import scala.annotation.tailrec
import scala.collection.mutable

object Day19 extends App {
  val input = 3014387

  object Part1 {
    type State = Array[Int]

    @tailrec
    def findNextWithGifts(state: State, current: Int): Int = {
      val next = if (current == (state.length - 1)) 0 else current + 1
      if (state(next) == 0)
        findNextWithGifts(state, next)
      else
        next
    }

    @tailrec
    def findResult(state: State, current: Int): Int = {
      val next = findNextWithGifts(state, current)

      val currentGifts = state(current)
      val newTotalGifts = currentGifts + state(next)
      state(current) = newTotalGifts
      state(next) = 0

      if (newTotalGifts == state.length)
        current
      else
        findResult(state, findNextWithGifts(state, current))
    }

    def run(): Unit = {
      val initialState: State = Stream.continually(1).take(input).toArray
      val result = findResult(initialState, 0)
      println(s"The winner is ${result + 1}")
    }
  }

  object Part2 {

    case class Elf(index: Int, gifts: Int)
    type State = mutable.ArrayBuffer[Elf]

    @tailrec
    def findResult(state: State, current: Int): Elf = {
      val currentSize = state.length
      if (currentSize % 1000 == 0) {
        println(((input - currentSize).toDouble / input.toDouble) * 100.0)
      }

      val stealFromIndex = (current + (currentSize / 2)) % currentSize
      val currentElf = state(current)
      val stealFrom = state(stealFromIndex)

      val newTotalGifts = currentElf.gifts + stealFrom.gifts
      if (newTotalGifts == input)
        currentElf
      else {
        state(current) = currentElf.copy(gifts = newTotalGifts)
        state.remove(stealFromIndex)

        val nextIndex = if (stealFromIndex < current) current else current + 1
        findResult(state, nextIndex % state.length)
      }
    }

    def run(): Unit = {
      val initialState = mutable.ArrayBuffer[Elf]((1 to input).map(Elf(_, 1)) : _*)
      val result = findResult(initialState, 0)
      println(s"The winner is $result")
    }
  }

  Part1.run()
  Part2.run()
}
