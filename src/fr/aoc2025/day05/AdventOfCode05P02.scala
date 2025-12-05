package fr.aoc2025.day05

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode05P02 {

  private def solve(input: String) : Long = {
    val lines = input.split("\n")
    val intervals = lines.map(_.strip()).takeWhile(_.nonEmpty).map(_.split("-").map(_.toLong)).map { x => (x(0), x(1)) }.toList
    solve(intervals)
  }

  private def mergeIntervals(intervals: List[(Long, Long)]): List[(Long, Long)] = {
    @tailrec
    def computeMergeIntervals(toMerge: List[(Long, Long)], solution: List[(Long, Long)]): List[(Long, Long)] = {
      toMerge match {
        case Nil => solution
        case (x, y)::others =>
        solution match {
          case Nil => computeMergeIntervals(others, List((x, y)))
          case (a, b)::sol if x <= b => computeMergeIntervals(others, (a, y.max(b))::sol)
          case (a, b)::sol => computeMergeIntervals(others, (x, y)::(a, b)::sol)
        }
      }
    }
    computeMergeIntervals(intervals.sorted, List())
  }

  private def solve(intervals: List[(Long, Long)]): Long = {
    mergeIntervals(intervals).map { case (l, r) => r - l + 1 }.sum
  }


  private def solveWithFile(filename: String): Long = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day05-example.txt"))
    println("Problem: " + solveWithFile("inputs/day05.txt"))
  }

}
