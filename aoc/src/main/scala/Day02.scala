import scala.io._

object Day02 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Shape(opponent: String, me: String)

  def parser(s: String): Shape =
    s match
      case s"$opponent $me"   => Shape(opponent, me)
      case _                  => sys.error("Unknown line")

  val input: List[Shape] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parser)
      .toList

  def calculateShapeScore(score: Int, shape: Shape): Int =
    shape.me match
      case "X" => score + 1 //Rock
      case "Y" => score + 2 //Paper
      case "Z" => score + 3 //Scissors

  def calculateRoundScore(score: Int, shape: Shape): Int =
    shape match
      case Shape("A","Y") | Shape("B","Z") | Shape("C","X") => score + 6
      case Shape("A","X") | Shape("B","Y") | Shape("C","Z") => score + 3
      case Shape("A","Z") | Shape("B","X") | Shape("C","Y") => score + 0

  def calculateNewRoundScore(score: Int, shape: Shape): Int =
    shape.me match
      case "X" => score + 0
      case "Y" => score + 3
      case "Z" => score + 6

  def calculateNewShapeScore(score: Int, shape: Shape): Int =
    shape match
      case Shape("A", "X") | Shape("C","Y") | Shape("B","Z")   => score + 3 //Scissors
      case Shape("B", "X") | Shape("A", "Y") | Shape("C", "Z") => score + 1 //Rock
      case Shape("C", "X") | Shape("B", "Y") | Shape("A", "Z") => score + 2 //Paper

  def execute(start: Int, calculate: (Int, Shape) => Int) =
    input.foldLeft(start)(calculate)

  val answer1: Int =
    execute(0, calculateShapeScore) + execute(0, calculateRoundScore)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    execute(0, calculateNewShapeScore) + execute(0, calculateNewRoundScore)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")