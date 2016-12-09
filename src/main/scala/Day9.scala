import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day9 extends App {

  case class Marker(length: Int, times: Int) {
    override def toString: String = s"(${length}x${times})"
  }

  def parseMarker(markerString: String): Option[Marker] = {
    val marker = """(\d+)x(\d+)""".r
    markerString match {
      case marker(length, times) =>
        Try {
          Marker(length.toInt, times.toInt)
        }.toOption
      case _ =>
        None
    }
  }

  def decompressV1(data: Seq[Char]): String = {
    sealed trait State
    case object Uncompressed extends State
    case class InMarker(marker: StringBuilder) extends State
    case class AfterMarker(marker: Marker, segment: StringBuilder) extends State

    @tailrec
    def process(builder: StringBuilder, data: Seq[Char], state: State): String = {
      data match {
        case head +: tail =>
          (state, head) match {
            case (Uncompressed, '(') =>
              process(builder, tail, InMarker(StringBuilder.newBuilder))
            case (Uncompressed, _) =>
              process(builder.append(head), tail, Uncompressed)

            case (InMarker(markerString), ')') =>
              parseMarker(markerString.toString) match {
                case Some(marker) =>
                  process(builder, tail, AfterMarker(marker, StringBuilder.newBuilder))

                case None =>
                  process(builder.append('(').append(markerString).append(')'), tail, Uncompressed)
              }
            case (InMarker(markerString), _) =>
              process(builder, tail, InMarker(markerString.append(head)))

            case (AfterMarker(marker, segment), _) if marker.length == (segment.length + 1) =>
              process(builder.append(segment.append(head).toString * marker.times), tail, Uncompressed)

            case (AfterMarker(marker, segment), _) =>
              process(builder, tail, AfterMarker(marker, segment.append(head)))
          }
        case _ =>
          builder.toString
      }
    }

    process(StringBuilder.newBuilder, data, Uncompressed)
  }

  def calculateLengthV2(data: Seq[Char]): Long = {
    case class StackedMarker(marker: Marker, segmentLength: Long, consumedLength: Long)

    sealed trait State {
      def length: Long
    }
    case class Uncompressed(override val length: Long) extends State
    case class InMarker(override val length: Long, markerStack: List[StackedMarker], marker: StringBuilder) extends State
    case class AfterMarker(override val length: Long, markerStack: List[StackedMarker], marker: Marker, segmentLength: Long, consumedLength: Long) extends State

    @tailrec
    def popFinished(state: State): State = {
      state match {
        case AfterMarker(length, markerStack, marker, segmentLength, consumedLength) =>
          if (marker.length == consumedLength) {
            val increase = segmentLength * marker.times
            markerStack.headOption match {
              case Some(parentMarker) =>
                popFinished(AfterMarker(
                  length,
                  markerStack.tail,
                  parentMarker.marker,
                  parentMarker.segmentLength + (segmentLength * marker.times),
                  parentMarker.consumedLength + marker.toString.length + consumedLength
                ))
              case None =>
                Uncompressed(length + increase)
            }
          } else {
            state
          }

        case _ =>
          state
      }
    }

    @tailrec
    def process(data: Seq[Char], state: State): Long = {
      data match {
        case head +: tail =>
          (state, head) match {
            case (Uncompressed(length), '(') =>
              process(tail, InMarker(length, List.empty, StringBuilder.newBuilder))
            case (Uncompressed(length), _) =>
              process(tail, Uncompressed(length + 1))

            case (InMarker(length, markerStack, markerString), ')') =>
              parseMarker(markerString.toString) match {
                case Some(marker) =>
                  process(tail, AfterMarker(length, markerStack, marker, 0, 0))

                case None if markerStack.isEmpty =>
                  process(tail, Uncompressed(length + markerString.length + 2))

                case None =>
                  process(
                    tail,
                    AfterMarker(
                      length,
                      markerStack.tail,
                      markerStack.head.marker,
                      markerStack.head.segmentLength + markerString.length + 2,
                      markerStack.head.consumedLength + markerString.length + 2))
              }
            case (InMarker(length, markerStack, markerString), _) =>
              process(tail, InMarker(length, markerStack, markerString.append(head)))

            case (AfterMarker(length, markerStack, marker, segmentLength, consumedLength), '(') =>
              process(tail, InMarker(length, StackedMarker(marker, segmentLength, consumedLength) :: markerStack, StringBuilder.newBuilder))

            case (AfterMarker(length, markerStack, marker, segmentLength, consumedLength), _) =>
              process(tail, popFinished(AfterMarker(length, markerStack, marker, segmentLength + 1, consumedLength + 1)))
          }
        case _ =>
          state.length
      }
    }

    process(data, Uncompressed(0))
  }


  val input = Source.fromResource("day9.txt").toList
  val decompressed = decompressV1(input)

  println(s"Length of v1-decompressed data is ${decompressed.length}")

  val v2Length = calculateLengthV2(input)
  println(s"Length of v2-decompressed data is $v2Length")
}
