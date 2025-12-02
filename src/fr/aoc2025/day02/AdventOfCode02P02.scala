package fr.aoc2025.day02

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode02P02 {

  private case class Interval(left: Long, right: Long) {
  }

  private def solve(input: String): Long = {
    val intervals = input.split(",").map(_.split("-").map(_.toLong)).map(x => Interval(x(0), x(1))).toList
    solve(intervals, 0)
  }

  @tailrec
  private def findPattern(target: String, divisor: Int): Boolean = {
    target.splitAt(target.length / divisor) match {
      case ("", _) => false
      case (x, _)  => x*divisor == target || findPattern(target, divisor + 1)
    }
  }

  @tailrec
  private def solve(intervals: List[Interval], acc: Long): Long = {
    intervals match {
      case Nil => acc
      case Interval(left, right)::others =>
        val n = (left to right)
          .filter(x => findPattern(x.toString, 2))
          .sum
        solve(others, acc + n)
    }
  }

  private def solveWithFile(filename: String): Long = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day02-example.txt"))
    println("Problem: " + solveWithFile("inputs/day02.txt"))
  }

}
