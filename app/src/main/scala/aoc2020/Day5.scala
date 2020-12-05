package aoc2020

import scala.io.Source

class Day5(inputFile: String) extends DaySolution {
  val input = Source
    .fromFile(inputFile)
    .getLines()
    .toList
    .map(new SeatIdentifier(_).seatID)
    .sorted
  def part1 = input.last.toString
  def part2 =
    input
      .sliding(2, 1)
      .find(l =>
        if (l.length > 1) {
          l(1) - l(0) > 1
        } else false)
      .map(_ match { case (x :: _) => x + 1 })
      .toString

  class SeatIdentifier(input: String) {
    val (rowDirs, colDirs) = (input.take(7), input.drop(7))
    val row = locate(rowDirs, (0 to 127).toList)
    val col = locate(colDirs, (0 to 7).toList)
    val seatID = row * 8 + col

    def locate(dirs: String, positions: List[Int]): Int =
      if (positions.length == 1) {
        positions(0)
      } else {
        dirs(0) match {
          case 'F' | 'L' =>
            locate(dirs.drop(1), positions.take(positions.length / 2))
          case 'B' | 'R' =>
            locate(dirs.drop(1), positions.drop(positions.length / 2))
        }
      }
  }
}
