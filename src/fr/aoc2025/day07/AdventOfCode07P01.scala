package fr.aoc2025.day07

import .solve

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode07P01 {

  private def solve(lines: List[String]): Long = {
    val start = lines.head.zipWithIndex.filter { case (c, _) => c == 'S' }.map { _._2 }.head
    val splitters = lines.drop(1).map { line => line.zipWithIndex.filter { case (c, _) => c == '^' }.map { _._2 }.toSet }
      .filter { _.nonEmpty }

    solve(Set(start), splitters)
  }

  private def spreadRay(ray: Int, splitters: Set[Int]): (Set[Int], Int) = {
    if (splitters.contains(ray)) (Set(ray - 1, ray + 1), 1) else (Set(ray), 0)
  }

  @tailrec
  private def solve(rays: Set[Int], splitters: List[Set[Int]], acc: Int = 0): Int = {
    splitters match {
      case Nil => acc
      case localSplitters::others =>
        val (newRays, nSplits) = rays.map { spreadRay(_, localSplitters) }.foldLeft((Set.empty[Int], 0)){ case ((s1, a), (s2, b)) =>  (s1 ++ s2, a + b) }
        solve(newRays -- localSplitters, others, acc + nSplits)
    }
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
