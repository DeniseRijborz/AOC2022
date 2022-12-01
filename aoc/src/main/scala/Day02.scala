import scala.io._

object Day02 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: String =
    Source
      .fromResource(s"input$day.txt").getLines.mkString

  println(input)

  val answer1: Int =
    5

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    5

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")