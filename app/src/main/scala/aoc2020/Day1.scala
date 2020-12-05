package aoc2020
import scala.io.Source
class Day1(inputFile: String) extends DaySolution {
  val input = Source.fromFile(inputFile).getLines().map(_.toInt).toList
  def part1: String =
    (for {
      x <- input
      y <- input
      if x + y == 2020
    } yield (x * y))(0).toString

  def part2: String = {
    (for {
      x <- input
      y <- input
      z <- input
      if x + y + z == 2020
    } yield (x * y * z))(0).toString
  }
}
