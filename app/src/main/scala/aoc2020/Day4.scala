package aoc2020

import scala.io.Source
import scala.util.{Success, Try}

class Day4(inputFile: String) extends DaySolution {
  val input = new Validator(Source.fromFile(inputFile).getLines().toList)
  def part1 = input.batchesWithNeededFields.length.toString
  def part2 = input.validBatches.toString

  class Validator(input: List[String]) {
    val batches = getBatches(input)
    def getBatches(strings: List[String]): List[String] = {
      if (strings.isEmpty) {
        List.empty
      } else {
        getBatches(strings.dropWhile(s => !s.isEmpty).drop(1))
          .appended(
            strings.takeWhile(s => !s.isEmpty).map(_.concat(" ")).mkString)
      }
    }
    val batchesWithNeededFields = batches.filter(
      s =>
        List("byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:").forall(
          s.contains(_)))
    val validBatches = batchesWithNeededFields.count(
      kvs =>
        kvs
          .split(' ')
          .map(
            _.dropWhile(_.isSpaceChar).takeWhile(!_.isSpaceChar).span(_ != ':'))
          .forall(_ match { case (k, v) => validateField(k, v.drop(1)) }))
    def validateField(key: String, value: String) = key match {
      case "byr" =>
        Try(value.toInt) match {
          case Success(n) => (n <= 2002 && n >= 1920)
          case _          => false
        }
      case "iyr" =>
        Try(value.toInt) match {
          case Success(n) => (n >= 2010 && n <= 2020)
          case _          => false
        }
      case "eyr" =>
        Try(value.toInt) match {
          case Success(n) => (n <= 2030 && n >= 2020)
          case _          => false
        }
      case "hgt" => {
        val height = Try(value.takeWhile(_.isDigit).toInt)
        val unit = value.dropWhile(_.isDigit)
        height match {
          case Success(n) =>
            unit match {
              case "cm" => (n <= 193 && n >= 150)
              case "in" => (n <= 76 && n >= 59)
              case _    => false
            }
          case _ => false
        }
      }
      case "hcl" =>
        if (value(0) == '#' && value.length == 7) {
          value.drop(1).forall("0123456789abcdef".contains(_))
        } else false
      case "ecl" =>
        List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
      case "pid" => value.length == 9 && value.forall(_.isDigit)
      case other => true
    }
  }

}
