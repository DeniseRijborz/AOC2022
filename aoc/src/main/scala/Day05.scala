import scala.io._
import scala.util.Try

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Procedure(move: Int, from: Int, to: Int)

  def parser(s: String): Procedure =
    s match
      case s"move $move from $from to $to" => Procedure(move.toInt, from.toInt, to.toInt)
      case _ => sys.error("Unknown format")

  val input: (List[String],List[String]) =
    Source
      .fromResource(s"input$day.txt").getLines.toList.splitAt(5)

  val instructions: List[Procedure] = input._2.map(parser)

  val crates: List[String] = input._1.dropRight(2)

//  val index: List[Int] = input._1.dropRight(1).takeRight(1).map(x => x.trim.toInt)
//  println(index)

  def parserCrates(s: String): IndexedSeq[(Int, Char)] =
    for (
      (c, i) <- s.zipWithIndex
    ) yield (i,c)

  val parsedCrates = crates.map(parserCrates).filter(_.nonEmpty).map(x => x.filter(_._2 != ' ')).map(x => x.filter(_._2 != '[')).map(x => x.filter(_._2 != ']'))
  println(input)
  println(instructions)
  println(parsedCrates)
  val test2 = (0 until parsedCrates.map(_.size).max).map(i => parsedCrates.map(s => Try(s(i)).getOrElse((0,' ')))).map(x => x.filter(_._1 != 0)).flatten.sortBy(_._1)

  def correct(original: (Int, Char)): (Int, Char) =
    original._1 match
      case 1 => (1, original._2)
      case 5 => (2, original._2)
      case 9 => (3, original._2)

  val test3 = test2.map(correct).groupBy(x => x._1)
  println(instructions.head)
  println(test3)
  println(test3(instructions.head.from)(0))
  println(test3 + (instructions.head.to -> crates(instructions.head.from)(0)))

  def moveCrate(procedure: List[Procedure], crates: Map[Int,IndexedSeq[(Int,Char)]]) =
    if procedure.isEmpty then crates
//    else
//    val newCrate = crates + (procedure.head.to -> crates(instructions.head.from)(0))
//    newcrate

//  def test(l: List[Any]) =
//    l match
//      case (x,y) =>

//  println(listCrates)
//  println(listCrates(-1 + instructions.head.from).takeRight(1))

//  def craneOperator(procedure: List[Procedure], crates: List[List[String]]) =
//    if procedure.isEmpty then crates
//    else
//      val intermediate: List[List[String]] = crates(-1 + procedure.head.from).takeRight(procedure.head.move) :+ crates(-1 + procedure.head.to)
//      val newCrate = intermediate(-1 + procedure.head.from).dropRight(procedure.head.move)

//  println(crates.map(parserCrates).filter(_.nonEmpty))
//  println(crates.map(x => if x.length > 0 then x.charAt(6)))

//  def moveCrates(instr: Procedure, crates: String) =


  val answer1: Int =
    5

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    5

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
