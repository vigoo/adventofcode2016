import scala.annotation.tailrec

object Day19 extends App {
  val input = 3014387

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

  val initialState: State = Stream.continually(1).take(input).toArray
  val result = findResult(initialState, 0)
  println(s"The winner is ${result + 1}")
}
