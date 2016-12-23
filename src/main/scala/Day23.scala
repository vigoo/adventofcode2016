import scala.annotation.tailrec
import scala.io.Source

object Day23 extends App {


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
  case class Jnz(condition: ValueOrRegister, delta: ValueOrRegister) extends Instruction
  case class Tgl(delta: ValueOrRegister) extends Instruction
  case object Nop extends Instruction

  type Program = Vector[Instruction]
  type Registers = Map[Register, Int]
  type IP = Int

  case class Machine(program: Program, ip: IP, registers: Registers) {
    def step(): Option[Machine] = {
      if (ip < program.length) {
        val (updatedProgram, updatedRegisters, deltaIp) = execute(program(ip))
        Some(
          this.copy(
            program = updatedProgram,
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

    private def getValue(valueOrRegister: ValueOrRegister): Int =
      valueOrRegister match {
        case Value(value) => value
        case Reg(reg) => registers(reg)
      }

    private def execute(instruction: Instruction): (Program, Registers, IP) = {
      instruction match {
        case Cpy(source, dest) => (program, registers.updated(dest, getValue(source)), 1)
        case Inc(register) => (program, registers.updated(register, registers(register) + 1), 1)
        case Dec(register) => (program, registers.updated(register, registers(register) - 1), 1)
        case Jnz(n, _) if getValue(n) == 0 => (program, registers, 1)
        case Jnz(n, delta) if getValue(n) != 0 => (program, registers, getValue(delta))
        case Tgl(delta) =>
          val target = ip + getValue(delta)
          if (target >= 0 && target < program.length) {
            val toggledInstruction =
              program(target) match {
                case Cpy(source, dest) => Jnz(source, Reg(dest))
                case Inc(register) => Dec(register)
                case Dec(register) => Inc(register)
                case Jnz(condition, Reg(register)) => Cpy(condition, register)
                case Jnz(_, Value(_)) => Nop
                case Tgl(Reg(register)) => Inc(register)
                case Tgl(Value(_)) => Nop
                case Nop => Nop
              }
            (program.updated(target, toggledInstruction), registers, 1)
          }
          else {
            (program, registers, 1)
          }
        case Nop => (program, registers, 1)
      }
    }
  }

  object Parser {
    private val cpy = """^cpy (-?\d+|[abcd]) ([abcd])$""".r
    private val inc = """^inc ([abcd])$""".r
    private val dec = """^dec ([abcd])$""".r
    private val jnz = """^jnz (-?\d+|[abcd]) (-?\d+|[abcd])$""".r
    private val tgl = """^tgl (-?\d+|[abcd])""".r

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
        case jnz(condition, d) =>
          val cond = parseRegister(condition) match {
            case Some(reg) => Reg(reg)
            case None => Value(condition.toInt)
          }
          val delta = parseRegister(d) match {
            case Some(reg) => Reg(reg)
            case None => Value(d.toInt)
          }
          Jnz(cond, delta)
        case tgl(d) =>
          val delta = parseRegister(d) match {
            case Some(reg) => Reg(reg)
            case None => Value(d.toInt)
          }
          Tgl(delta)
        case _ =>
          throw new IllegalArgumentException(s"Could not parse line: $line")
      }
    }
  }

  def initialRegisterState: Registers =
    Map(RegisterA -> 7, RegisterB -> 0, RegisterC -> 0, RegisterD -> 0)

  def initialRegisterState2: Registers =
    Map(RegisterA -> 12, RegisterB -> 0, RegisterC -> 0, RegisterD -> 0)

  val exampleProgram = """cpy 2 a
                         |tgl a
                         |tgl a
                         |tgl a
                         |cpy 1 a
                         |dec a
                         |dec a""".stripMargin.lines.toVector.map(Parser.parseInstruction)
  val initialExampleState = Machine(exampleProgram, 0, initialRegisterState)
  val finalExampleState = initialExampleState.run()
  finalExampleState.dump()


  val program = Source.fromResource("day23.txt").getLines.toVector.map(Parser.parseInstruction)
  val initialState = Machine(program, 0, initialRegisterState)
  val finalState = initialState.run()
  finalState.dump()

  val initialState2 = Machine(program, 0, initialRegisterState2)
  val finalState2 = initialState2.run()
  finalState2.dump()
}
