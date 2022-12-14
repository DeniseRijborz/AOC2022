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
      .toList
      .filter(_.nonEmpty)
      .grouped(2)
      .toList

  val input1 = input.map(x => x(0))
  val input2 = input.map(x => x(1))

  def compare(first: String, second: String): Boolean =
//    println(first)
//    println(second)
    if first.isEmpty then true
    else if second.isEmpty then false
    else if first.head.isDigit && first.tail.head.isDigit && second.head.isDigit && first.slice(0,2).toInt > second.head.asDigit then false
    else if first.head.isDigit && first.tail.head.isDigit && second.head.isDigit && first.slice(0,2).toInt < second.head.asDigit then true
    else if second.head.isDigit && second.tail.head.isDigit && first.head.isDigit && second.slice(0,2).toInt > first.head.asDigit then true
    else if second.head.isDigit && second.tail.head.isDigit && first.head.isDigit && second.slice(0,2).toInt < first.head.asDigit then false
    else if first.head.isDigit && second.head.isDigit && first.head.asDigit > second.head.asDigit then false
    else if first.head.isDigit && second.head.isDigit && first.head.asDigit < second.head.asDigit then true
    else if first.head.isDigit && second.head.isDigit == false && second.head == '[' && second.tail.head == ']' then false
    else if second.head.isDigit && first.head.isDigit == false && first.head == '[' && first.tail.head == ']' then true
    else if second.head.isDigit && first.head.isDigit == false then compare(first.tail,second)
    else if first.head.isDigit && second.head.isDigit == false then compare(first,second.tail)
    else compare(first.tail, second.tail)


  def compareInput(): List[Boolean] =
    for (
      (x,y) <- (input1 zip input2)
    ) yield compare(x,y)

  val size = input1.length
  val compare = compareInput()
  println(compare)
  val index = (1 to size).toList
  val count = compare.zip(index).filter(x => x._1 == true).map(x => x._2).foldLeft(0)((a,b)=> a + b)
  println(index)
  println(compare)

  val answer1: Int =
    compare.zip(index).filter(x => x._1 == true).map(x => x._2).foldLeft(0)((a,b)=> a + b)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    5

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
