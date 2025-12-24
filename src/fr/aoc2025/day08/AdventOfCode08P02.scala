package fr.aoc2025.day08

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AdventOfCode08P02 {

  private case class Box(x: Long, y: Long, z: Long) {}

  private class UnionFind(boxes: List[Box]) {
    private val boxToIndex = boxes.zipWithIndex.toMap
    private val uf = boxes.indices.toArray
    var clusterCount: Int = boxes.length


    def merge(first: Box, second: Box): Unit = {
      val firstRoot = find(first)
      val secondRoot = find(second)
      if (firstRoot != secondRoot) {
        uf(boxToIndex(secondRoot)) = boxToIndex(firstRoot)
        clusterCount -= 1
      }
    }

    private def find(box: Box): Box = {
      @tailrec
      def loop(index: Int): Int = {
        if (uf(index) ==  index) index else loop(uf(index))
      }
      boxes(loop(boxToIndex(box)))
    }

    def clusters(): List[List[Box]] = {
      boxes.groupBy { find }.values.toList
    }

  }

  private def solve(lines: List[String]): Long = {
    val boxes = lines.map { _.split { "," }.map { _.toLong } }
      .map { array => Box(array(0), array(1), array(2)) }
    solveProblem(boxes)
  }

  private def squaredDistance(first: Box, second: Box): Long = {
    (first.x - second.x)*(first.x - second.x) +
      (first.y - second.y)*(first.y - second.y) +
      (first.z - second.z)*(first.z - second.z)
  }

  private def closestBox(targetIndex: Int, boxes: List[Box]): List[(Box, Box, Long)] = {
    (0 until targetIndex)
       .filter { _ != targetIndex }
       .map { boxes }
       .map { t => (boxes(targetIndex), t, squaredDistance(boxes(targetIndex), t)) }
       .toList
  }

  private def solveProblem(boxes: List[Box]): Long = {
    val uf = new UnionFind(boxes)

    @tailrec
    def loop(boxesLeft: List[(Box, Box)], output: Long = 0): Long = {
        boxesLeft match {
          case Nil => output
          case (first, second)::others =>
            uf.merge(first, second)
            if (uf.clusterCount == 1) {
              loop(Nil, first.x*second.x)
            } else {
              loop(others, output)
            }
        }
      }

    val orderedBoxes = boxes.indices.drop(1).flatMap { closestBox(_, boxes) }
      .sortBy { _._3 }
      .map { t => (t._1, t._2) }
      .toList
    loop(orderedBoxes)
  }

  private def solveWithFile(filename: String): Long = {
    Using(Source.fromFile(filename)) { fd =>
      solve(fd.mkString.stripTrailing.split("\n").map { _.strip }.toList)
    }.get
  }

  def main(args: Array[String]) : Unit = {
    println("Example: " + solveWithFile("inputs/day08-example.txt"))
    println("Problem: " + solveWithFile("inputs/day08.txt"))
  }
}
