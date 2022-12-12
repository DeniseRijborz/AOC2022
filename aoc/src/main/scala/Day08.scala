import scala.io._

object Day08 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: List[List[Int]] =
    Source
      .fromResource(s"input$day.txt").getLines.map(_.toList.map(_.asDigit)).toList

  val size: Int = input.head.length

  val grid = List.range(0, size).flatMap(x => List.range(0, size).map(y => (x, y)))
  val leftRight: List[List[(Int,(Int,Int))]] = (input.flatten zip grid).grouped(size).toList
  val upDown: List[List[(Int,(Int,Int))]] = (input.flatten zip grid).grouped(size).toList.transpose
  val rightLeft: List[List[(Int,(Int,Int))]] = (input.flatten zip grid).grouped(size).toList.map(x => x.reverse).reverse
  val downUp: List[List[(Int,(Int,Int))]] = (input.flatten zip grid).grouped(size).toList.map(x => x.reverse).reverse.transpose

  def visibleTrees(trees: List[(Int,(Int,Int))], highestTrees: List[(Int,(Int,Int))], currentHighest: Int): List[(Int,(Int,Int))] =
    if trees.isEmpty then highestTrees
    else if trees.head._1 > currentHighest then
      val newHighestTrees: List[(Int,(Int,Int))] = highestTrees :+ trees.head
      visibleTrees(trees.tail, newHighestTrees, trees.head._1)
    else visibleTrees(trees.tail, highestTrees, currentHighest)

  val highTrees1 = leftRight.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees2 = upDown.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees3 = rightLeft.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees4 = downUp.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees: List[(Int,(Int,Int))] = highTrees1.concat(highTrees2).concat(highTrees3).concat(highTrees4)

  val answer1: Int =
    highTrees.distinct.size

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    5

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
