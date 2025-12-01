package fr.aoc2025.day01

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode01P2 {

  private val EXAMPLE : String = """
    L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82""".stripMargin.stripIndent.stripLeading

  private def solve(input: String): Unit = {
    println(solve(input.split("\n").map {
      case s"L$n" => -n.toInt
      case s"R$n" => n.toInt
    }.toList, 50, 0))
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


  def main(args: Array[String]) : Unit = {
    if (args.isEmpty) {
      solve(EXAMPLE)
    } else {
      Using(Source.fromFile(args.apply(0))) { fd =>
        solve(fd.mkString)
      }
    }
  }
}
