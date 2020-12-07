package aoc2020
import scala.io.Source
class Day3(inputFile: String) extends DaySolution {
  val input = Source.fromFile(inputFile).getLines().toVector
  val width = input(0).length

  def part1: String = check(3, 1).toString
  def part2: String =
    List(check(1, 1),
         check(3, 1),
         check(5, 1),
         check(7, 1),
         check(1, 2)).product.toString

  def check(xStep: Int, yStep: Int): BigInt = {
    input
      .sliding(1, yStep)
      .map(_(0))
      .zipWithIndex
      .count(_ match { case (line, ind) => line((ind * xStep) % width) == '#' })
  }
}

