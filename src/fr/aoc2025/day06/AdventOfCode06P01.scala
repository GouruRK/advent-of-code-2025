package fr.aoc2025.day06

import scala.io.Source
import scala.util.Using

object AdventOfCode06P01 {

  private def solve(str: String): Long = {
    val lines = str.split("\n").map(_.strip()).reverse
    val values = lines.drop(1)
      .flatMap {
        _.split(" ")
          .filter { _.nonEmpty }
          .map { _.toLong }
          .zipWithIndex
      }
      .groupBy { case (_, b) => b }
      .toList
      .sortBy { _._1 }
      .map { _._2.map { _._1 } }
    val operators = lines.head.split(" ").filter(_.nonEmpty).map {
      case "+" => ((a: Long, b: Long) => a + b, 0L)
      case "*" => ((a: Long, b: Long) => a * b, 1L)
    }.toList
    solve(values, operators)
  }

  private def solve(valueList: List[Array[Long]], operators: List[((Long, Long) => Long, Long)]): Long = {
    valueList.zip(operators).map { case (values, operator) => values.fold(operator._2){ operator._1 } }.sum
  }

  private def solveWithFile(filename: String): Long = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day06-example.txt"))
    println("Problem: " + solveWithFile("inputs/day06.txt"))
  }
}
