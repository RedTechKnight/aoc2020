package aoc2020

import scala.io.Source

class Day2(inputFile: String) {
  val input = Source.fromFile(inputFile).getLines().toList
  def part1: String =
    input
      .count(line =>
        new Validator(line).isValidPassword(line.dropWhile(_ != ':').drop(2)))
      .toString
  def part2: String =
    input
      .count(
        line =>
          new Validator(line)
            .isValidPasswordPart2(line.dropWhile(_ != ':').drop(2)))
      .toString
  def printSolution = println(s"Part 1: ${part1}\nPart 2: ${part2}")

}

class Validator(pattern: String) {
  val min: Int = pattern.takeWhile(_.isDigit).toInt
  val max: Int = pattern.dropWhile(_.isDigit).drop(1).takeWhile(_.isDigit).toInt
  val targetChar: Char = pattern.dropWhile(_ != ' ').drop(1)(0)

  def isValidPassword(password: String): Boolean =
    (min <= password.count(_ == targetChar)
      && password.count(_ == targetChar) <= max)

  def isValidPasswordPart2(password: String): Boolean =
    (password(min - 1) == targetChar) != (password(max - 1) == targetChar)
}
