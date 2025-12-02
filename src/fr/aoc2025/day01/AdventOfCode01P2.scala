package fr.aoc2025.day01

import scala.io.Source
import scala.util.Using

object AdventOfCode01P2 {

  private def solve(input: String): Int = {
    solve(input.split("\n").map { _.strip }.map {
      case s"L$n" => -n.toInt
      case s"R$n" => n.toInt
    }.toList, 50, 0)
  }

  private def truemod(n: Int, m: Int) : Int = {
    ((n % m) + m) % m
  }

  private def solve(lines: List[Int], current: Int, acc: Int): Int = {
    lines match {
      case Nil => acc
      case n::others =>
        var next = current + truemod(n.abs, 100)*n.sign
        if (next <= 0 || next >= 100) {
          next = truemod(next, 100)
          if (next != current && current != 0) {
            return solve(others, next, acc + n.abs / 100 + 1)
          }
        }
        solve(others, next, acc + n.abs / 100)
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
