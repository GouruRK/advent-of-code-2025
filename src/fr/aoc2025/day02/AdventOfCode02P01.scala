package fr.aoc2025.day02

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode02P01 {

  private case class Interval(left: Long, right: Long) {
  }

  private val EXAMPLE : String = """
    11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"""
    .stripMargin.stripIndent.stripLeading

  private def solve(input: String): Unit = {
    val intervals = input.split(",").map(_.split("-").map(_.toLong)).map(x => Interval(x(0), x(1))).toList
    println(solve(intervals, 0))
  }

  private def findNextEvenLengthNumber(n: Long): Long = {
    val len = n.toString.length
    if (len % 2 == 0) n else Math.pow(10, len).toLong
  }

  @tailrec
  private def solve(intervals: List[Interval], acc: Long): Long = {
    intervals match {
      case Nil => acc
      case Interval(left, right)::others =>
        val current = findNextEvenLengthNumber(left).toString
        val (firstHalf, _) = current.splitAt(current.length / 2)
        val n = Iterator.iterate(firstHalf.toLong){_ + 1}
          .map(x => (x.toString * 2).toLong)
          .dropWhile(_ <= left)
          .takeWhile(_ <= right)
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
