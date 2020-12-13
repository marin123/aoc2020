package solutions

import scala.io.Source

class Solution13(str: String) extends Solution {
  val input: Array[String] =
    Source.fromResource(s"data/$str").getLines().toArray
  val bus: Int = input(0).toInt
  val lines: Array[String] = input(1).split(",")
  override def firstPart(): Int = {
    val workingLines: Array[Int] = lines.filter(_ != "x").map(_.toInt)
    val firstLine = workingLines.map(x => (x, bus / x, x - bus % x)).minBy(_._3)
    println(firstLine)
    firstLine._1 * firstLine._3
  }

  override def secondPart(): Int = {
    val diff: Array[Int] = lines.indices.toArray
    val diffZipLines = lines.zip(diff).filter(_._1 != "x")
    diffZipLines.map(println)
    0
  }

  override def getSolution(): (Int, Int) = (firstPart(), secondPart())
}
