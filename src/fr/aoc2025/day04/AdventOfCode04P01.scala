package fr.aoc2025.day04

import scala.io.Source
import scala.util.Using

object AdventOfCode04P01 {

  private val PLAIN = '@'
  private val EMPTY = '.'

  private val ARROUND = Array(
    (-1, 0), (-1, -1), (0, -1), (1, -1),
    (1, 0), (1, 1), (0, 1), (-1, 1)
  )

  private case class Grid(grid: Array[String]) {
    val width: Int  = grid(0).length
    val height: Int = grid.length

    def at(coord: (Int, Int)): Char = {
      coord match {
        case (x, _) if x < 0 || x >= width => EMPTY
        case (_, y) if y < 0 || y >= height => EMPTY
        case (x, y) => grid(y)(x)
      }
    }
  }

  private def coords(grid: Grid): IndexedSeq[(Int, Int)] = {
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
    } yield (x, y)
  }

  private def solve(grid: Grid): Int = {
    coords(grid).filter(grid.at(_) == PLAIN).map { case (i, j) =>
      ARROUND.map { case (x, y) => (x + i, y + j) }
        .map(grid.at)
        .count(e => e == PLAIN)
    } .map { x => if (x < 4) 1 else 0}
      .sum
  }

  private def solve(input: String): Int = {
    solve(Grid(input.split("\n").map(_.strip)))
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
