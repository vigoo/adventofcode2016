import Day23.Machine

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends App {

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
  case class Out(x: ValueOrRegister) extends Instruction
  case object Nop extends Instruction

  type Program = Vector[Instruction]
  type Registers = Map[Register, Int]
  type IP = Int

  case class Machine(program: Program, ip: IP, registers: Registers, output: Vector[Int]) {
    def step(): Option[Machine] = {
      if (ip < program.length) {
        val (updatedProgram, updatedRegisters, deltaIp, newOutput) = execute(program(ip))
        Some(
          this.copy(
            program = updatedProgram,
            ip = ip + deltaIp,
            registers = updatedRegisters,
            output = newOutput match {
              case Some(data) => output :+ data
              case None => output
            }
          )
        )
      } else {
        None
      }
    }

    @tailrec
    final def run(maxOutput: Int): Machine = {
      step() match {
        case Some(nextStep) if nextStep.output.length < maxOutput =>
          nextStep.run(maxOutput)
        case Some(nextStep) =>
          nextStep
        case None =>
          this
      }
    }

    def dump(): Unit = {
      println(s"IP = $ip")
      registers.foreach { case (reg, value) =>
        println(s"$reg = $value")
      }
    }

    private def getValue(valueOrRegister: ValueOrRegister): Int =
      valueOrRegister match {
        case Value(value) => value
        case Reg(reg) => registers(reg)
      }

    private def execute(instruction: Instruction): (Program, Registers, IP, Option[Int]) = {
      instruction match {
        case Cpy(source, dest) => (program, registers.updated(dest, getValue(source)), 1, None)
        case Inc(register) => (program, registers.updated(register, registers(register) + 1), 1, None)
        case Dec(register) => (program, registers.updated(register, registers(register) - 1), 1, None)
        case Jnz(n, _) if getValue(n) == 0 => (program, registers, 1, None)
        case Jnz(n, delta) if getValue(n) != 0 => (program, registers, getValue(delta), None)
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
            (program.updated(target, toggledInstruction), registers, 1, None)
          }
          else {
            (program, registers, 1, None)
          }
        case Out(x) => (program, registers, 1, Some(getValue(x)))
        case Nop => (program, registers, 1, None)
      }
    }
  }

  object Parser {
    private val cpy = """^cpy (-?\d+|[abcd]) ([abcd])$""".r
    private val inc = """^inc ([abcd])$""".r
    private val dec = """^dec ([abcd])$""".r
    private val jnz = """^jnz (-?\d+|[abcd]) (-?\d+|[abcd])$""".r
    private val tgl = """^tgl (-?\d+|[abcd])""".r
    private val out = """^out (-?\d+|[abcd])""".r

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
        case out(x) =>
          val value = parseRegister(x) match {
            case Some(reg) => Reg(reg)
            case None => Value(x.toInt)
          }
          Out(value)
        case _ =>
          throw new IllegalArgumentException(s"Could not parse line: $line")
      }
    }
  }

  def tryWith(program: Program, a: Int): Boolean = {
    println(s"Running with $a...")
    val initialRegisterState: Registers =
      Map(RegisterA -> a, RegisterB -> 0, RegisterC -> 0, RegisterD -> 0)

    val initialState = Machine(program, 0, initialRegisterState, Vector.empty)
    val finalState = initialState.run(maxOutput = 10)

    finalState.output == Vector(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  }

  val program = Source.fromResource("day25.txt").getLines.toVector.map(Parser.parseInstruction)
  val firstResult = Stream.from(0).filter(tryWith(program, _)).head
  println(s"Value $firstResult seems good")
}
