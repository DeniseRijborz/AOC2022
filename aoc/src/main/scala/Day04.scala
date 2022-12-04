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

  val input: List[Pairs] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parser)
      .toList

  def actualListFirst(pairs: Pairs): IndexedSeq[Int] =
    for (
      x <- pairs.a.toInt to pairs.b.toInt
    ) yield x

  def actualListSecond(pairs: Pairs): IndexedSeq[Int] =
    for (
      y <- pairs.c.toInt to pairs.d.toInt
    ) yield y

  val first: List[IndexedSeq[Int]] = input.map(actualListFirst)
  val second: List[IndexedSeq[Int]] = input.map(actualListSecond)
  val pairs: List[(IndexedSeq[Int],IndexedSeq[Int])] = first.zip(second)

  def contains(p: (IndexedSeq[Int],IndexedSeq[Int])): Boolean =
    p._1.forall(x => p._2.contains(x)) | p._2.forall(x => p._1.contains(x))

  def overlap(p: (IndexedSeq[Int],IndexedSeq[Int])): IndexedSeq[Boolean] =
    for (
      x <- p._1
      if p._2.contains(x)
    ) yield true

  val answer1: Int =
    pairs.map(contains).count(_ == true)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    pairs.map(overlap).map(x => x.nonEmpty).count(_ == true)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")