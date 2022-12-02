import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: List[String] =
    Source
      .fromResource(s"input$day.txt").getLines.toList

  case class Elves(elf: Int, amountOfCalories: Int)

  def calculateCaloriesPerElf(calories: List[String], current: Elves, elves: List[Elves]): List[Elves] =
    if calories.isEmpty then elves
    else if calories.head.nonEmpty then
      val newAmount = Elves(current.elf, current.amountOfCalories + calories.head.toInt)
      calculateCaloriesPerElf(calories.tail, newAmount, elves :+ newAmount)
    else if calories.head.isEmpty then
      val newElf = Elves(current.elf + 1, 0)
      calculateCaloriesPerElf(calories.tail, newElf, elves :+ newElf)
    else sys.error("Unknown input")

  val listElves: List[Elves] = calculateCaloriesPerElf(input, Elves(1,0), List())

  val answer1: Int =
    listElves
      .maxBy(_.amountOfCalories)
      .amountOfCalories

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    listElves
      .groupBy(_.elf)
      .view
      .mapValues(value => value.map(_.amountOfCalories).max)
      .toList
      .sortWith(_._2 > _._2)
      .take(3)
      .foldLeft(0)(_+_._2)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
