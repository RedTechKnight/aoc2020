package aoc2020

import scala.io.Source

class Day6(inputFile: String) extends DaySolution {
  val input = Source.fromFile(inputFile).getLines().toList
  def part1 = getGroups(input).map(yesAnswersAny).sum.toString
  def part2 = getGroups(input).map(yesAnswersAll).sum.toString

  def getGroups(strings: List[String]): List[List[String]] = {
    if (strings.isEmpty) {
      List.empty
    } else {
      getGroups(strings.dropWhile(s => !s.isEmpty).drop(1))
        .appended(
          strings.takeWhile(s => !s.isEmpty))
    }
  }

  def yesAnswersAny(group: List[String]): Int = group.mkString.distinct.length
  def yesAnswersAll(group: List[String]): Int = ('a' to 'z').toList.filter(c => group.forall(_.contains(c))).length
 }
