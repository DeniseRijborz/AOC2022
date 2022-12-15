import scala.io._

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Pairs(a: String, b: String, c: String, d: String)

  def parser(s: String): Pairs =
    s match
      case s"${a}-${b},${c}-${d}" => Pairs(a, b, c, d)
      case _ => sys.error("Unknown format")

  def range(p: Pairs): (Range, Range) =
    val range1 = Range.inclusive(p.a.toInt, p.b.toInt)
    val range2 = Range.inclusive(p.c.toInt, p.d.toInt)
    (range1, range2)

  val input: List[(Range,Range)] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parser)
      .map(range)
      .toList

  def contains(range1: Range, range2: Range): Boolean =
    range1.forall(x => range2.contains(x)) | range2.forall(x => range1.contains(x))

  def overlap(range1: Range, range2: Range): Boolean =
    range1.exists(x => range2.contains(x))

  val answer1: Int =
    input.map(contains).count(_ == true)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    input.map(overlap).count(_ == true)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")