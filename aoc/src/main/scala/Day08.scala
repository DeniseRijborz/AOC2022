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

  val grid: List[(Int,Int)] = List.range(0, size).flatMap(x => List.range(0, size).map(y => (x, y)))
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

  val highTrees1: List[(Int,(Int,Int))] = leftRight.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees2: List[(Int,(Int,Int))] = upDown.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees3: List[(Int,(Int,Int))] = rightLeft.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees4: List[(Int,(Int,Int))] = downUp.map(x => visibleTrees(x, List(), -1)).flatten
  val highTrees: List[(Int,(Int,Int))] = highTrees1.concat(highTrees2).concat(highTrees3).concat(highTrees4)

  val gridPart2: List[(Int, Int)] = leftRight.flatten.map(x => x._2)
  val coordinates: List[Int] = leftRight.flatten.map(x => x._1)
  val inputPart2: Map[(Int, Int), Int] = (gridPart2 zip coordinates).toMap

  def scenicScore(tree: (Int,Int)): Int =
    val pathLeft: List[Int] = (0 to tree._1).map(x => inputPart2(x,tree._2)).toList.reverse
    val pathRight: List[Int] = (tree._1 until size).map(x => inputPart2(x,tree._2)).toList
    val pathTop: List[Int] = (0 to tree._2).map(y => inputPart2(tree._1,y)).toList.reverse
    val pathBottom: List[Int] = (tree._2 until size).map(y => inputPart2(tree._1,y)).toList
    def score(todo: List[Int]): Int =
      val index = todo.tail.indexWhere(_ >= todo.head)
      if index != -1 then index + 1 else todo.tail.length
    val vl = score(pathLeft)
    val vr = score(pathRight)
    val vt = score(pathTop)
    val vb = score(pathBottom)
    vl * vr * vt * vb

  val answer1: Int =
    highTrees.distinct.size

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    inputPart2.keys.map(scenicScore).max

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
