package fr.aoc2025.day03

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode03P01 {

  private def solve(input: String): Int = {
    solve(input.split("\n").map { _.strip }.map { x => x.chars().map { _ - '0' }.toArray.toList }.toList)
  }

  private def indexOfMax(list : List[Int]) : (Int, Int) = {
    @tailrec
    def indexOfMax(l: List[(Int, Int)], maxElement: Int, maxIndex: Int): (Int, Int) = {
      l match {
        case Nil => (maxElement, maxIndex)
        case (a, _)::others if a <= maxElement => indexOfMax(others, maxElement, maxIndex)
        case (a, i)::others => indexOfMax(others, a, i)
      }
    }
    indexOfMax(list.zipWithIndex, -1, -1)
  }

  @tailrec
  private def solve(banks: List[List[Int]], acc: Int = 0): Int = {
    banks match {
      case Nil => acc
      case bank::others =>
        val (a, i) = indexOfMax(bank)
        val (subList, compute) = if (i == bank.size - 1) (bank.slice(0, i), (b: Int) => b*10 + a)
            else (bank.slice(i + 1, bank.size), (b: Int) => a*10 + b)
        val (b, _) = indexOfMax(subList)
        solve(others, acc + compute(b))
    }
  }

  private def solveWithFile(filename: String): Long = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day03-example.txt"))
    println("Problem: " + solveWithFile("inputs/day03.txt"))
  }

}
