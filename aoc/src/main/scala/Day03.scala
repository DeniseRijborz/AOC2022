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

  def commonLetter(compartments: (String, String)): List[Char] =
    val letters: List[String] = compartments._1.sliding(1).toList
    for (
      x <- letters;
      y <- x
      if compartments._2.contains(y)
    ) yield y

  def priority(list: List[Char]): List[Int] =
    val alphabetLower = "abcdefghijklmnopqrstuvwxyz".zip(Stream.from(1)).toList
    val alphabetUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".zip(Stream.from(27)).toList
    val alphabet: List[(Char,Int)] = alphabetLower ++ alphabetUpper
    for (
      (x,y) <- alphabet;
      z <- list
      if z == x
    ) yield y

  def commonLetterGroup(compartments: List[String]): List[Char] =
    val letters: List[Char] = compartments(0).sliding(1).toList.flatten
    for (
      y <- letters
      if compartments(1).contains(y) && compartments(2).contains(y)
    ) yield y

  val answer1: Int =
    priority(
      input
        .map(parser)
        .map(commonLetter)
        .map(m => m.distinct)
        .flatten
    ).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    priority(
      input
        .sliding(3, 3)
        .toList
        .map(commonLetterGroup)
        .map(m => m.distinct)
        .flatten
    ).sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
