import scala.io._

object Day03 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  def parser(s: String): (String,String) =
    s.splitAt(s.length / 2)

  def commonLetter(compartments: (String, String)): Set[Char] =
    compartments._1.toSet.intersect(compartments._2.toSet)

  def priority(c: Char): Int =
    if c.isLower then c.toInt - 96 else c.toInt - 38

  def commonLetterGroup(compartments: List[String]): List[Char] =
    val letters: List[Char] = compartments(0).sliding(1).toList.flatten
    for (
      y <- letters
      if compartments(1).contains(y) && compartments(2).contains(y)
    ) yield y

  val answer1 =
    input
      .map(parser)
      .map(commonLetter)
      .flatten
      .map(priority)
      .sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    input
      .sliding(3, 3)
      .toList
      .map(commonLetterGroup)
      .map(m => m.distinct)
      .flatten
      .map(priority)
      .sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
