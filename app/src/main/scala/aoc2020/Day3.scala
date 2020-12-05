package aoc2020
import scala.io.Source
class Day3(inputFile: String) extends DaySolution {
  val input = new Grid(Source.fromFile(inputFile).getLines().toList)
  def part1: String = input.check(3, 1).toString
  def part2: String =
    List(input.check(1, 1),
         input.check(3, 1),
         input.check(5, 1),
         input.check(7, 1),
         input.check(1, 2)).product.toString
}

class Grid(source: List[String]) {
  val grid = source
  val width = source(0).length

  def check(xStep: Int, yStep: Int): BigInt = {
    grid
      .sliding(1, yStep)
      .map(_(0))
      .zipWithIndex
      .count(_ match { case (line, ind) => line((ind * xStep) % width) == '#' })
  }
}
