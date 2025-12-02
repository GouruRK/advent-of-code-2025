package fr.aoc2025.day02

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode02P01 {

  private case class Interval(left: Long, right: Long) {
  }

  private def solve(input: String): Long = {
    val intervals = input.split(",").map(_.split("-").map(_.toLong)).map(x => Interval(x(0), x(1))).toList
    solve(intervals, 0)
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
