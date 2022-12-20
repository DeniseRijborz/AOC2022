import scala.io._
import scala.util.Try

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: String =
    Source
      .fromResource(s"input$day.txt").getLines.mkString

  def distinctChar(n: Int): Int =
    input
      .indexOf(
        input
          .sliding(n)
          .map(x => x.distinct)
          .filter(_.length == n)
          .toList
          .head
      ) + n

  val answer1: Int =
    distinctChar(4)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    distinctChar(14)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")