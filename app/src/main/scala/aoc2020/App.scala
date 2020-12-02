/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package aoc2020

import aoc2020.Day1
import aoc2020.Day2
import scala.util.{Try, Success, Failure}
object App {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Specify a number to run that day's solution.")
    } else {
      Try(args(0).toInt) match {
        case Success(1) => {
          val day = new Day1("../inputs/1")
          println(
            s"Solutions for Day 1\nPart 1: ${day.part1}\nPart 2: ${day.part2}")
        }
        case Success(2) => {
          val day = new Day2("../inputs/2")
          println(
            s"Solutions for Day 2\nPart 1: ${day.part1}\nPart 2: ${day.part2}")
        }
        case Success(_) => {
          println(s"Only days 1 - 2 have solutions currently.")
        }
        case Failure(_) => {
          println("Enter a valid number.")
        }
      }
    }
  }
}
