import scala.io._

object Day10 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Signal(cmd: String, value: Int, cycle: Int)

    def parser(s: String): Signal =
      s match
        case s"addx $value" => Signal("add", value.toInt, 2)
        case s"noop"        => Signal("noop", 0, 1)

  val input: List[Signal] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parser)
      .toList

  def instructions(signals: List[Signal], cycle: Int, value: Int, total: List[(Int,Int)]): List[(Int,Int)] =
    if signals.isEmpty then total
    else if signals.head.cmd == "add" then
      val newCycle = cycle + signals.head.cycle
      val newValue = value + signals.head.value
      instructions(signals.tail, newCycle, newValue, total :+ (cycle + 1, value) :+ (newCycle,newValue))
    else if signals.head.cmd == "noop" then
      val newCycle = cycle + signals.head.cycle
      val newValue = value + signals.head.value
      instructions(signals.tail, newCycle, newValue, total :+ (newCycle, newValue))
    else sys.error("Error")

  val start: List[(Int,Int)] = List((1,1))
  val signals: List[Int] = List(20,60,100,140,180,220)
  val cycles: List[(Int,Int)] = instructions(input, 1, 1, start)
  val interestingSignals: List[(Int,Int)] = cycles.filter(x => signals.contains(x._1))

  def image(cycle: (Int,Int), pixel: String): String =
    val sprite: List[Int] = List(cycle._2 - 1, cycle._2, cycle._2 + 1)
    val position: Int = (cycle._1 - 1) % 40
    if sprite.contains(position) then pixel + "█"
    else pixel + " "

  val answer1: Int =
    interestingSignals.map(x => x._1 * x._2).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2 =
    cycles.map(x => image(x, "")).grouped(40).toList.foreach(println)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
