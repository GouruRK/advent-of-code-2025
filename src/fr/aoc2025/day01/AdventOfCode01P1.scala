package fr.aoc2025.day01

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode01P1 {

  private def solve(input: String): Int = {
    solve(input.split("\n").map { _.strip } .map {
      case s"L$n" => -n.toInt
      case s"R$n" => n.toInt
    }.toList, 50, 0)
  }

  @tailrec
  private def solve(lines: List[Int], current: Int, acc: Int): Int = {
    lines match {
      case Nil => acc
      case n::others =>
        val next = (current - n) % 100
        solve(others, next, acc + (if (next == 0) 1 else 0))
    }
  }

  private def solveWithFile(filename: String): Long = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day01-example.txt"))
    println("Problem: " + solveWithFile("inputs/day01.txt"))
  }

}
