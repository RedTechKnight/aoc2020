package aoc2020

import scala.io.Source

class Day2(inputFile: String) extends DaySolution {
  val input = Source.fromFile(inputFile).getLines().map(validate).toList
  def part1: String = input.count(_._1).toString
  def part2: String = input.count(_._2).toString

  def validate(pattern: String): (Boolean, Boolean) = {
    val password = pattern.dropWhile(_ != ':').drop(2)
    val min: Int = pattern.takeWhile(_.isDigit).toInt
    val max: Int =
      pattern.dropWhile(_.isDigit).drop(1).takeWhile(_.isDigit).toInt
    val targetChar: Char = pattern.dropWhile(_ != ' ').drop(1)(0)

    val part1 =
      (min <= password.count(_ == targetChar)
        && password.count(_ == targetChar) <= max)

    val part2 =
      (password(min - 1) == targetChar) != (password(max - 1) == targetChar)

    (part1, part2)
  }
}
