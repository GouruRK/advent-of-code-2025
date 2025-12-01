package fr.aoc2025.day01

import java.util
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode01P1 {

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
    L82""".stripIndent.stripLeading

  private def solve(input: String): Unit = {
    println(solve(input.split("\n").map {
      case s"L$n" => -n.toInt
      case s"R$n" => n.toInt
    }.toList, 50, 0))
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
