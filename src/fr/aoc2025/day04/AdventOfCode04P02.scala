package fr.aoc2025.day04

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode04P02 {

  private val PLAIN = '@'
  private val EMPTY = '.'

  private val ARROUND = Array(
    (-1, 0), (-1, -1), (0, -1), (1, -1),
    (1, 0), (1, 1), (0, 1), (-1, 1)
  )

  private def solve(input: String): Int = {
    solve(input.split("\n").map(_.strip)
      .zipWithIndex.flatMap { case (str, y) =>
        str.zipWithIndex
          .filter { case (c, _) => c == PLAIN }
          .map { case (_, x) => (x, y) }
      }.toSet)
  }

  @tailrec
  private def solve(rolls: Set[(Int, Int)], acc: Int = 0): Int = {
    val newRolls = rolls.map { case (i, j) =>
      (i, j, ARROUND.map { case (x, y) => (x + i, y + j) }
        .count { rolls.contains })
    } .filter { _._3 >= 4 }
      .map { x => (x._1, x._2) }

    if (newRolls.size != rolls.size) {
      solve(newRolls, acc + rolls.size - newRolls.size)
    } else {
      acc
    }
  }

  private def solveWithFile(filename: String): Int = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day04-example.txt"))
    println("Problem: " + solveWithFile("inputs/day04.txt"))
  }

}
