package aoc2020

import scala.io.Source

class Day7(inputFile: String) extends DaySolution {
  val input = Source
    .fromFile(inputFile)
    .getLines()
    .map(l => l.filterNot(",.".contains(_)))

  def part1 = totalCanHoldGold(canHoldGold, cantHoldGold).size.toString
  def part2 = totalInside(bags("shiny gold")).toString

  val bags = input.foldLeft(Map.empty[String, Array[BagContent]])((map, line) =>
    parseLine(line) match { case (k, v) => map.updated(k, v) })

  val (canHoldGold, cantHoldGold) = bags.partition(_ match {
    case (_, arr) => !arr.find(_.colour == "shiny gold").isEmpty
  })

  def totalCanHoldGold(holdDirect: Map[String, Array[BagContent]],
                       holdIndirect: Map[String, Array[BagContent]])
    : Map[String, Array[BagContent]] = {
    val (direct, indirect) = holdIndirect.partition(_ match {
      case (k, v) => !v.map(_.colour).intersect(holdDirect.keys.toSeq).isEmpty
    })
    if (direct.isEmpty) {
      holdDirect
    } else {
      totalCanHoldGold(holdDirect.concat(direct), indirect)
    }
  }
  def totalInside(innerBags: Array[BagContent]): Int =
    if (innerBags.isEmpty) 0
    else
      innerBags
        .map(bag => bag.count + bag.count * totalInside(bags(bag.colour)))
        .sum

  def bagContents(inp: Array[String]) =
    inp.map(content => {
      val line = content.dropWhile(!_.isDigit)
      val count = line.takeWhile(_.isDigit).toInt
      val bagColour = line.dropWhile(c => c.isDigit || c.isSpaceChar)
      BagContent(count, bagColour.strip())
    })

  def parseLine(inp: String) = {
    val words = inp.split("bag")
    val containingBagColour = words(0).strip()
    val containingBagContents = bagContents(
      words.filter(!_.filter(_.isDigit).isEmpty))
    (containingBagColour, containingBagContents)
  }

  case class BagContent(count: Int, colour: String)
}
