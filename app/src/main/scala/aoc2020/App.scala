/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package aoc2020
import scala.util.{Try, Success}
object App {
  def main(args: Array[String]): Unit = {
    val days = List(
      () => new Day1("../inputs/1").printSolution,
      () => new Day2("../inputs/2").printSolution,
      () => new Day3("../inputs/3").printSolution,
      () => new Day4("../inputs/4").printSolution,
      () => new Day5("../inputs/5").printSolution,
      () => new Day6("../inputs/6").printSolution,
      () => new Day7("../inputs/7").printSolution,
      () => new Day8("../inputs/8").printSolution
    )
    if (args.length < 1) {
      println(
        s"Enter a number between ${1} and ${days.length} to run that day's solutions, 0 for all of them")

    } else {
      Try(args(0).toInt) match {
        case Success(0) =>
          for { (day, ind) <- days.zipWithIndex } yield {
            println(s"Solution for Day ${ind + 1}")
            day()
          }
        case Success(n) if n <= days.length => days(n - 1)()
        case _ =>
          println(
            s"Enter a number between ${1} and ${days.length} to run that day's solutions, 0 for all of them")
      }
    }
  }
}

trait DaySolution {
  def part1: String
  def part2: String
  def printSolution = println(s"Part 1: ${part1}\nPart 2: ${part2}")
}
