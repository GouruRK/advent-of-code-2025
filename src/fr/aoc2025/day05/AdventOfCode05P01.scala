package fr.aoc2025.day05

import scala.io.Source
import scala.util.Using

object AdventOfCode05P01 {

  private def solve(input: String) : Int = {
    val lines = input.split("\n")
    val intervals = lines.map(_.strip()).takeWhile(_.nonEmpty).map(_.split("-").map(_.toLong)).map { x => (x(0), x(1)) }.toList
    val values = lines.map(_.strip()).dropWhile(_.nonEmpty).drop(1).map(_.toLong).toList
    solve(intervals, values)
  }

  private def solve(intervals: List[(Long, Long)], values: List[Long]): Int = {
    values.map { x =>
      intervals.exists { case (l, r) => l <= x && x <= r }
    }.count(x => x)
  }


  private def solveWithFile(filename: String): Int = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day05-example.txt"))
    println("Problem: " + solveWithFile("inputs/day05.txt"))
  }

}
