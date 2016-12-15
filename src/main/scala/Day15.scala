object Day15 extends App {
  /*
  Disc #1 has 17 positions; at time=0, it is at position 1.
  Disc #2 has 7 positions; at time=0, it is at position 0.
  Disc #3 has 19 positions; at time=0, it is at position 2.
  Disc #4 has 5 positions; at time=0, it is at position 0.
  Disc #5 has 3 positions; at time=0, it is at position 0.
  Disc #6 has 13 positions; at time=0, it is at position 5.
   */

  case class DeltaTime(dt: Int) extends AnyVal {
    def -(other: DeltaTime): DeltaTime = DeltaTime(dt - other.dt)
    def normalize(cycleTime: Int): DeltaTime =
      if (dt < 0) {
        DeltaTime(cycleTime + dt).normalize(cycleTime)
      } else {
        this
      }
  }

  case class DiscId(id: Int) extends AnyVal {
    def timeToReach: DeltaTime = DeltaTime(id) // time to reach the disc from top
  }
  case class Time(t: Int) extends AnyVal {
    def next(): Time = Time(t + 1)
    def advance(by: DeltaTime): Time = Time(t + by.dt)
  }

  case class Disc(id: DiscId, positions: Int, current: Int) {
    def advance(by: DeltaTime): Disc =
      this.copy(current = (current + by.dt) % positions)

    def timeToZero: DeltaTime =
      DeltaTime(if (current == 0) 0 else positions - current)

    def timeToGoodStart: DeltaTime =
      (timeToZero - id.timeToReach).normalize(positions)

    override def toString: String =
      s"D${id.id}($positions; $current; $timeToZero; $timeToGoodStart"
  }

  case class State(discs: Set[Disc], time: Time)

  implicit val stepOrdering = new Ordering[DeltaTime] {
    override def compare(x: DeltaTime, y: DeltaTime): Int = x.dt.compareTo(y.dt)
  }

  def nextState(state: State): State = {
    val delta = DeltaTime(1)

    val nextState = State(
      discs = state.discs.map(disc => disc.advance(delta)),
      time = state.time.advance(delta)
    )

    nextState
  }

  def isSolution(state: State): Boolean =
    state.discs.forall(disc => disc.timeToGoodStart.dt == 0)

  val initialState: State = State(
    discs = Set(
      Disc(DiscId(1), 17, 1),
      Disc(DiscId(2), 7, 0),
      Disc(DiscId(3), 19, 2),
      Disc(DiscId(4), 5, 0),
      Disc(DiscId(5), 3, 0),
      Disc(DiscId(6), 13, 5)
    ),
    time = Time(0)
  )

  def firstPart(): Unit = {
    val states = Stream.iterate(initialState)(nextState)
    val solution = states.find(isSolution)
    println(s"First solution is $solution")
  }

  def secondPart(): Unit = {
    val states = Stream.iterate(initialState.copy(discs = initialState.discs + Disc(DiscId(7), 11, 0)))(nextState)
    val solution = states.find(isSolution)
    println(s"Second solution is $solution")
  }

  firstPart()
  secondPart()
}
