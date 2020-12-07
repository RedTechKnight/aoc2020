package aoc2020

import scala.io.Source

class Day6(inputFile: String) extends DaySolution {
  val input =
    Source.fromFile(inputFile).mkString.filter(_ != '\r').split("\n\n")
  def part1 =
    input
      .map(s => s.filter('a'.to('z').contains(_)).distinct.length)
      .sum
      .toString
  def part2 =
    input
      .map(s =>
        ('a' to 'z').filter(c => s.split('\n').forall(_.contains(c))).length)
      .sum
      .toString

}
