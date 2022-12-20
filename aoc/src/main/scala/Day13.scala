import scala.io._

object Day13 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: List[List[String]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.replace("10","A"))
      .toList
      .filter(_.nonEmpty)
      .grouped(2)
      .toList

  val left: List[String] = input.map(x => x(0))
  val right: List[String] = input.map(x => x(1))

  def compare(left: String, right: String): Boolean =
    if left.head == ']' && right.head != ']' then true //left list runs out of items first
    else if right.head == ']' && left.head != ']' then false //right list runs out of items first
    else if left.head == '[' && (right.head.isDigit || right.head == 'A') then compare(left.tail, right.head + "]" + right.tail) //if left item is a list and right an int, convert the right to a list
    else if right.head == '[' && (left.head.isDigit || left.head == 'A') then compare(left.head + "]" + left.tail, right.tail) //if right item is a list and left an int, convert the left to a list
    else if left.head < right.head then true //left int is lower then right
    else if left.head > right.head then false //left int is higher then right
    else compare(left.tail,right.tail) //left.head == right.head

  def compareInput(): List[Boolean] =
    for (
      (x, y) <- left zip right
    ) yield compare(x, y)

  val numberOfPairs: Int = left.length
  val compare: List[Boolean] = compareInput()
  val index: List[Int] = (1 to numberOfPairs).toList

  val add1: String = "[[2]]"
  val add2: String = "[[6]]"
  val left2: List[String] = left :+ add1
  val right2: List[String] = right :+ add2
  val sorted: List[String] = left2.concat(right2).sortWith(compare)

  val answer1: Int =
    compare.zip(index).filter(x => x._1 == true).map(x => x._2).foldLeft(0)((a,b) => a + b)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    (sorted.indexOf(add1) + 1) * (sorted.indexOf(add2) + 1)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
