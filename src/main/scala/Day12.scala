import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App {

  sealed trait Register
  case object RegisterA extends Register
  case object RegisterB extends Register
  case object RegisterC extends Register
  case object RegisterD extends Register

  sealed trait ValueOrRegister
  case class Value(value: Int) extends ValueOrRegister
  case class Reg(register: Register) extends ValueOrRegister

  sealed trait Instruction
  case class Cpy(source: ValueOrRegister, dest: Register) extends Instruction
  case class Inc(register: Register) extends Instruction
  case class Dec(register: Register) extends Instruction
  case class Jnz(condition: ValueOrRegister, delta: Int) extends Instruction

  type Program = Vector[Instruction]
  type Registers = Map[Register, Int]
  type IP = Int

  case class Machine(program: Program, ip: IP, registers: Registers) {
    def step(): Option[Machine] = {
      if (ip < program.length) {
        val (updatedRegisters, deltaIp) = execute(program(ip))
        Some(
          this.copy(
            ip = ip + deltaIp,
            registers = updatedRegisters
          )
        )
      } else {
        None
      }
    }

    @tailrec
    final def run(): Machine = {
      step() match {
        case Some(nextStep) =>
          nextStep.run()
        case None =>
          this
      }
    }

    def dump(): Unit = {
      println(s"IP = $ip")
      registers.foreach { case (reg, value) =>
        println(s"$reg = ${value}")
      }
    }

    private def execute(instruction: Instruction): (Registers, IP) = {
      instruction match {
        case Cpy(Value(n), dest) => (registers.updated(dest, n), 1)
        case Cpy(Reg(source), dest) => (registers.updated(dest, registers(source)), 1)
        case Inc(register) => (registers.updated(register, registers(register) + 1), 1)
        case Dec(register) => (registers.updated(register, registers(register) - 1), 1)
        case Jnz(Value(n), delta) if n == 0 => (registers, 1)
        case Jnz(Value(n), delta) if n != 0 => (registers, delta)
        case Jnz(Reg(register), delta) if registers(register) == 0 => (registers, 1)
        case Jnz(Reg(register), delta) if registers(register) != 0 => (registers, delta)
      }
    }
  }

  object Parser {
    private val cpy = """^cpy (-?\d+|[abcd]) ([abcd])$""".r
    private val inc = """^inc ([abcd])$""".r
    private val dec = """^dec ([abcd])$""".r
    private val jnz = """^jnz (-?\d+|[abcd]) (-?\d+)$""".r

    private def parseRegister(s: String): Option[Register] = {
      s match {
        case "a" => Some(RegisterA)
        case "b" => Some(RegisterB)
        case "c" => Some(RegisterC)
        case "d" => Some(RegisterD)
        case _ => None
      }
    }

    def parseInstruction(line: String): Instruction = {
      line match {
        case cpy(source, dest) =>
          val src = parseRegister(source) match {
            case Some(reg) => Reg(reg)
            case None => Value(source.toInt)
          }
          Cpy(src, parseRegister(dest).get)
        case inc(reg) =>
          Inc(parseRegister(reg).get)
        case dec(reg) =>
          Dec(parseRegister(reg).get)
        case jnz(condition, delta) =>
          val cond = parseRegister(condition) match {
            case Some(reg) => Reg(reg)
            case None => Value(condition.toInt)
          }
          Jnz(cond, delta.toInt)
        case _ =>
          throw new IllegalArgumentException(s"Could not parse line: $line")
      }
    }
  }

  def initialRegisterState: Registers =
    Map(RegisterA -> 0, RegisterB -> 0, RegisterC -> 0, RegisterD -> 0)

  def initialRegisterState2: Registers =
    Map(RegisterA -> 0, RegisterB -> 0, RegisterC -> 1, RegisterD -> 0)

  val program = Source.fromResource("day12.txt").getLines.toVector.map(Parser.parseInstruction)
  val initialState = Machine(program, 0, initialRegisterState)
  val finalState = initialState.run()
  finalState.dump()

  val initialState2 = Machine(program, 0, initialRegisterState2)
  val finalState2 = initialState2.run()
  finalState2.dump()
}
