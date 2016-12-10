import scala.io.Source

object Day10 extends App {

  case class Value(n: Int) extends AnyVal

  sealed trait Target
  case class Bot(id: Int) extends Target
  case class Output(id: Int) extends Target

  sealed trait Instruction
  case class Distribute(bot: Bot, lowerTo: Target, higherTo: Target) extends Instruction
  case class Assign(value: Value, to: Bot) extends Instruction

  case class BotState(distribute: Distribute, values: Set[Value])
  case class State(bots: Map[Bot, BotState], outputs: Map[Output, Value])

  val distribute = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r
  val assign = """value (\d+) goes to bot (\d+)""".r

  def parseInstruction(row: String): Instruction = {
    row match {
      case distribute(botId, lowTarget, lowId, highTarget, highId) =>
        Distribute(
          Bot(botId.toInt),
          if (lowTarget == "bot") Bot(lowId.toInt) else Output(lowId.toInt),
          if (highTarget == "bot") Bot(highId.toInt) else Output(highId.toInt)
        )
      case assign(value, botId) =>
        Assign(Value(value.toInt), Bot(botId.toInt))
      case _ =>
        throw new IllegalArgumentException(s"Could not parse row: $row")
    }
  }

  def buildInitialState(instructionList: List[Instruction]): State = {
    val distributions = instructionList.collect { case d: Distribute => d }
    val botMap = distributions.map(d => d.bot -> BotState(d, Set.empty)).toMap

    val assignments = instructionList.collect { case a: Assign => a }
    State(
      assignments.foldLeft(botMap) { case (m, assignment) =>
          m.updated(assignment.to, m(assignment.to).copy(values = m(assignment.to).values + assignment.value))
      },
      Map.empty
    )
  }

  def updateBotState(state: State, bot: Bot, f: BotState => BotState): State = {
    state.copy(bots = state.bots.updated(bot, f(state.bots(bot))))
  }

  def addValueTo(value: Value)(botState: BotState): BotState = {
    require(botState.values.size < 2)
    botState.copy(values = botState.values + value)
  }

  def transfer(state: State, value: Value, target: Target): State = {
    println(s"Transfering $value to $target")

    target match {
      case bot@Bot(_) =>
        updateBotState(state, bot, addValueTo(value))
      case output@Output(id) =>
        state.copy(outputs = state.outputs.updated(output, value))
    }
  }

  def clearValues(state: State, bot: Bot): State = {
    println(s"Removing values from $bot")
    updateBotState(state, bot, botState => botState.copy(values = Set.empty))
  }

  def botStep(state: State, bot: Bot): State = {
    val botState = state.bots(bot)
    assert(botState.values.size == 2)

    val instruction = botState.distribute
    val lowValue = botState.values.minBy(_.n)
    val highValue = botState.values.maxBy(_.n)

    println(s"$bot is processing $lowValue and $highValue")

    val state0 = transfer(state, lowValue, instruction.lowerTo)
    val state1 = transfer(state0, highValue, instruction.higherTo)
    clearValues(state1, bot)
  }

  def step(state: State): State = {
    val botsWithTwoValues = state.bots.collect { case (bot, BotState(_, values)) if values.size == 2 => bot }.toList
    println(s"Stepping $botsWithTwoValues")
    botsWithTwoValues.foldLeft(state)(botStep)
  }

  val instructionList = Source.fromResource("day10.txt").getLines.toList.map(parseInstruction)
  val initialState = buildInitialState(instructionList)

  var states: Set[State] = Set.empty
  var current = step(initialState)
  while (!states.contains(current)) {
    states = states + current
    current = step(current)
  }

  val output0 = current.outputs(Output(0))
  val output1 = current.outputs(Output(1))
  val output2 = current.outputs(Output(2))

  println(s"Output 0: $output0")
  println(s"Output 1: $output1")
  println(s"Output 2: $output2")
  println(s"Result: ${output0.n*output1.n*output2.n}")
}
