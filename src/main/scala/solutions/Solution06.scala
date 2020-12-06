package solutions

import scala.io.Source

class Solution06(inputFile: String) extends Solution {
  val input: Array[String] =
    Source
      .fromResource(s"data/$inputFile")
      .getLines
      .mkString("\n")
      .split("\n\n")

  override def firstPart(): Int = {
    val noQuestions = input
      .map(_.replace("\n", ""))
      .map(_.split(""))
      .map(_.toSet)
      .map(_.size)
    noQuestions.sum
  }

  override def secondPart(): Int = {
    val intersectionQuestions = input
      .map(_.replace("\n", " "))
      .map(_.split(" "))
      .map(x => x.map(y => y.split("").toSet).reduce(_.intersect(_)).size)
    intersectionQuestions.sum
  }

  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }
}
