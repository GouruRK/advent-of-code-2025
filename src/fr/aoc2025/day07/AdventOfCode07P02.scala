package fr.aoc2025.day07

import scala.io.Source
import scala.util.Using

object AdventOfCode07P02 {

  private def solve(lines: List[String]): Long = {
    val grid = lines.map { line =>
      line.map {
        case 'S' => 1
        case '.' => 0
        case '^' => -1
      }.map { _.toLong }
        .toArray
    }.toArray
    solve(grid)
  }

  private def solve(grid: Array[Array[Long]]): Long = {
    for (y <- 0 until (grid.length - 1)) {
      for (x <- grid(y).indices)  {
        if (grid(y)(x) != -1) {
          grid(y + 1)(x) match {
            case -1 =>
              grid(y + 1)(x - 1) += grid(y)(x)
              grid(y + 1)(x + 1) += grid(y)(x)
            case _ => grid(y + 1)(x) += grid(y)(x)
          }
        }
      }
    }
    grid.last.sum
  }

  private def solveWithFile(filename: String): Long = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing.split("\n").map(_.strip).toList)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day07-example.txt"))
    println("Problem: " + solveWithFile("inputs/day07.txt"))
  }
}
