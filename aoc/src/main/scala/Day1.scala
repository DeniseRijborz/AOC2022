object Day1 extends App:
  import scala.io.Source

  val lines: String =
    Source
      .fromResource("Day1")
      .getLines
      .mkString
