import scala.collection.immutable.Set
import scala.collection.mutable

object Day11 extends App {

  /*
  The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
  The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
  The third floor contains nothing relevant.
  The fourth floor contains nothing relevant.
   */

  trait Solution {
    type Element

    def elementCount: Int

    def initialState: State

    case class Floor(number: Int)

    sealed trait Item {
      def elem: Element
    }

    case class Generator(elem: Element) extends Item {
      override def toString: String = s"${elem}G"
    }

    case class Microchip(elem: Element) extends Item {
      override def toString: String = s"${elem}M"
    }

    type FloorState = Set[Item]

    case class ElevatorState(currentFloor: Floor, nextFloor: Floor, items: Set[Item])

    case class State(elevatorState: ElevatorState, floors: Map[Floor, FloorState])

    def dumpState(state: State): Unit = {
      for (floor <- (4 to(1, -1)).map(Floor)) {
        print(s"F${floor.number} ")
        if (state.elevatorState.nextFloor == floor) {
          print("E(")
          print(state.elevatorState.items.mkString(" "))
          print(") ")
        } else {
          print(".           ")
        }

        state.floors(floor).foreach { item =>
          print(item)
          print(" ")
        }
        println()
      }
    }

    def compatible(a: Item, b: Item): Boolean = {
      if (a != b) {
        (a, b) match {
          case (Generator(e1), Microchip(e2)) => e1 == e2
          case (Microchip(e1), Generator(e2)) => e1 == e2
          case (Generator(_), Generator(_)) => true
          case (Microchip(_), Microchip(_)) => true
        }
      } else {
        true
      }
    }

    def allCompatible(items: Set[Item]): Boolean = {
      val byElement =
        items.foldLeft(Map.empty[Element, Set[Item]].withDefaultValue(Set.empty)) { case (m, item) =>
            m.updated(item.elem, m(item.elem) + item)
        }
      val nonConnectedOrGenerator =
        byElement
          .filter { case (_, s) => s.size == 1 }
          .map { case (_, s) => s.head }
          .toSet
          .union(items.filter {
            case _: Generator => true
            case _ => false
          })

      val incompatibles = for {
        first <- nonConnectedOrGenerator
        second <- nonConnectedOrGenerator
        if !compatible(first, second)
      } yield (first, second)

      incompatibles.isEmpty
    }

    def itemsOnCurrentFloor(state: State): Set[Item] =
      state.floors(state.elevatorState.nextFloor).union(state.elevatorState.items)

    def isSolution(state: State): Boolean = {
      val floor = state.elevatorState.nextFloor
      val items = itemsOnCurrentFloor(state).size
      floor == Floor(4) && items == elementCount * 2
    }

    def validElevatorMoves(state: State): Set[ElevatorState] = {
      val items = itemsOnCurrentFloor(state)
      val nextFloors = (state.elevatorState.currentFloor, state.elevatorState.nextFloor) match {
        case (Floor(3), Floor(4)) => Set(Floor(3))
        case (Floor(2), Floor(1)) => Set(Floor(2))
        case (Floor(_), Floor(b)) => Set(Floor(b - 1), Floor(b + 1))
      }

      val possibilities = for {
        nextFloor <- nextFloors
        first <- items
        second <- items
        if compatible(first, second)
        if allCompatible(state.floors(nextFloor).union(Set(first, second)))
        if allCompatible(state.floors(state.elevatorState.currentFloor).diff(Set(first, second)))
      } yield ElevatorState(state.elevatorState.nextFloor, nextFloor, Set(first, second))

      possibilities
    }

    def loadItems(state: State, elevatorState: ElevatorState): State = {
      state.copy(
        floors = state.floors.updated(
          elevatorState.currentFloor,
          state.floors(elevatorState.currentFloor).diff(elevatorState.items)))
    }

    def unloadItems(state: State, elevatorState: ElevatorState): State = {
      state.copy(
        floors = state.floors.updated(
          elevatorState.nextFloor,
          state.floors(elevatorState.nextFloor).union(elevatorState.items)))
    }

    def applyMove(state: State, move: ElevatorState): State = {
      loadItems(unloadItems(state, state.elevatorState), move).copy(elevatorState = move)
    }

    def estimatedCost(state: State): Int = {
      state.floors(Floor(3)).size * 2 +
      state.floors(Floor(2)).size * 4 +
      state.floors(Floor(1)).size * 6 +
      Math.abs(state.elevatorState.nextFloor.number - 4)
    }

    case class Step(state: State, history: List[State], cost: Int) {
      def fullPath: List[State] = (state :: history).reverse
      def stepCount: Int = history.size

      def next(move: ElevatorState): Step = {
        val nextState = applyMove(state, move)
        val estimation = stepCount + estimatedCost(nextState)
        Step(nextState, state :: history, estimation)
      }
    }

    implicit val stepOrdering = new Ordering[Step] {
      override def compare(x: Step, y: Step): Int =
        -x.cost.compareTo(y.cost)
    }

    def findSolution(initialState: State): Option[Int] = {
      val steps: mutable.PriorityQueue[Step] = mutable.PriorityQueue(Step(initialState, List.empty, 0))
      val visited: mutable.Set[State] = mutable.Set.empty
      var result: Option[Int] = None
      var reportTick: Long = System.currentTimeMillis()

      while (steps.nonEmpty && result.isEmpty) {
        val step = steps.dequeue

        val tick = System.currentTimeMillis()
        if ((tick - reportTick) > 2000) {
          println(s"Current step count: ${step.stepCount}; estimation: ${step.cost}; queue length: ${steps.length}; visited states: ${visited.size}")
          reportTick = tick
        }

        if (isSolution(step.state)) {
          println("Found result with steps:")
          step.fullPath.foreach { s =>
            dumpState(s)
            println("---")
          }

          result = Some(step.stepCount)
        } else {
          if (!visited.contains(step.state)) {
            val validMoves = validElevatorMoves(step.state)
            for (move <- validMoves) {
              steps.enqueue(step.next(move))
            }
          }
        }
        visited += step.state
      }

      result
    }
  }

  object Example extends Solution {
    sealed trait Element
    case object Hydrogen extends Element { override def toString: String = "H" }
    case object Lithium extends Element { override def toString: String = "L" }

    val elementCount = 2

    val initialState = State(
      ElevatorState(Floor(2), Floor(1), Set.empty),
      Map(
        Floor(1) -> Set(Microchip(Hydrogen), Microchip(Lithium)),
        Floor(2) -> Set(Generator(Hydrogen)),
        Floor(3) -> Set(Generator(Lithium)),
        Floor(4) -> Set.empty)
    )

    def run(): Unit = {
      val numberOfSteps = findSolution(initialState)
      println(s"Result: $numberOfSteps")
    }
  }

  trait First extends Solution {
    sealed trait Element
    case object Polonium extends Element { override def toString: String = "Po" }
    case object Thulium extends Element { override def toString: String = "Th" }
    case object Promethium extends Element { override def toString: String = "Pr" }
    case object Ruthenium extends Element { override def toString: String = "Ru" }
    case object Cobalt extends Element { override def toString: String = "Co" }

    def elementCount = 5

    def initialState = State(
      ElevatorState(Floor(2), Floor(1), Set.empty),
      Map(
        Floor(1) -> Set(Generator(Polonium), Generator(Thulium), Microchip(Thulium), Generator(Promethium), Generator(Ruthenium), Microchip(Ruthenium), Generator(Cobalt), Microchip(Cobalt)),
        Floor(2) -> Set(Microchip(Polonium), Microchip(Promethium)),
        Floor(3) -> Set.empty,
        Floor(4) -> Set.empty
      )
    )

    def run(): Unit = {
      val numberOfSteps = findSolution(initialState)
      println(s"Result: $numberOfSteps")
    }
  }

  object First extends First

  object Second extends First {
    case object Elerium extends Element { override def toString: String = "El" }
    case object Dilithium extends Element { override def toString: String = "Di" }

    override def elementCount = 7

    override def initialState: Second.State =
      super.initialState.copy(
        floors = super.initialState.floors.updated(
          Floor(1),
          super.initialState.floors(Floor(1)).union(
            Set(Generator(Elerium), Microchip(Elerium), Generator(Dilithium), Microchip(Dilithium))
          )))
  }

  Second.dumpState(Second.initialState)
  Second.run()
}
