import scala.io._

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
      .fromResource(s"input$day.txt").getLines.toList.splitAt(10)

  val stacksOfCrates: Map[Int, List[Char]] =
    Map(1 -> List('R','N','P','G'),
      2 -> List('T','J','B','L','C','S','V','H'),
      3 -> List('T','D','B','M','N','L'),
      4 -> List('R','V','P','S','B'),
      5 -> List('G','C','Q','S','W','M','V','H'),
      6 -> List('W','Q','S','C','D','B','J'),
      7 -> List('F','Q','L'),
      8 -> List('W','M','H','T','D','L','F','V'),
      9 -> List('L','P','B','V','M','J','F'))

  val instructions: List[Procedure] = input._2.map(parser)

  def moveCrate(procedure: List[Procedure], crates: Map[Int, List[Char]], reverse: Boolean): String =
    if procedure.isEmpty then crates.toSeq.sortBy(_._1).map(x => x._2.last).mkString
    else
      val size: Int = procedure.head.move
      val from: Int = procedure.head.from
      val to: Int = procedure.head.to
      val removeFromCrate: List[Char] = crates(from).dropRight(size)
      val addToCrate: List[Char] =
        if reverse then crates(to) ++ crates(from).takeRight(size).reverse
        else crates(to) ++ crates(from).takeRight(size)
      val updatedCrates: Map[Int, List[Char]] = crates.updated(from, removeFromCrate).updated(to, addToCrate)
      moveCrate(procedure.tail, updatedCrates, reverse)

  val answer1: String =
    moveCrate(instructions, stacksOfCrates, true)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: String =
    moveCrate(instructions, stacksOfCrates, false)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
