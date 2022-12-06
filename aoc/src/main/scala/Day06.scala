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

  val fourChar = input.sliding(4).map(x => x.distinct).filter(_.length == 4).toList.head
  val fourteenChar = input.sliding(14).map(x => x.distinct).filter(_.length == 14).toList.head

  val answer1: Int =
    input.indexOf(fourChar) + 4

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    input.indexOf(fourteenChar) + 14

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")