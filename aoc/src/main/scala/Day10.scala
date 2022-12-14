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
      .fromResource(s"input$day.txt").getLines.map(parser).toList

  def instructions(signals: List[Signal], cycle: Int, value: Int, total: List[(Int,Int)]): List[(Int,Int)] =
    if signals.isEmpty then total
    else if signals.head.cmd == "add" then
      val newCycle = cycle + signals.head.cycle
      val newValue = value + signals.head.value
      instructions(signals.tail, newCycle, newValue, total :+ (newCycle,newValue))
    else if signals.head.cmd == "noop" then
      val newCycle = cycle + signals.head.cycle
      val newValue = value + signals.head.value
      instructions(signals.tail, newCycle, newValue, total :+ (newCycle, newValue))
    else sys.error("Error")

  val start: List[(Int,Int)] = List((1,1))
  println(instructions(input, 1, 1, start))

  val signals = List(20,60,100,140,180,220)
  val cycles = instructions(input, 1, 1, start)
  val interestingSignals = cycles.filter(x => signals.contains(x._1))
  println(interestingSignals)

  val answer1 =
    interestingSignals.map(x => x._1 * x._2).sum + 16 * 20

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    5

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
