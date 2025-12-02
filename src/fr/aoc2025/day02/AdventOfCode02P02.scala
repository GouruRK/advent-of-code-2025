package fr.aoc2025.day02

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode02P02 {

  private case class Interval(left: Long, right: Long) {
  }

  private val EXAMPLE : String = """
    11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"""
    .stripMargin.stripIndent.stripLeading

  private def solve(input: String): Unit = {
    val intervals = input.split(",").map(_.split("-").map(_.toLong)).map(x => Interval(x(0), x(1))).toList
    println(solve(intervals, 0))
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

  def main(args: Array[String]) : Unit = {
    if (args.isEmpty) {
      solve(EXAMPLE)
    } else {
      Using(Source.fromFile(args.apply(0))) { fd =>
        solve(fd.mkString.stripTrailing)
      }
    }
  }

}
