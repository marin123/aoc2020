package solutions

import scala.io.Source

class Solution03(inputFile: String) extends Solution {
  val input: Array[Array[String]] =
    Source.fromResource(s"data/$inputFile").getLines.toArray.map(_.split(""))
  val x_max: Int = input(0).length
  val y_max: Int = input.length
  def get_sum(step_x: Int, step_y: Int): Int = {
    var step: Int = 0
    var sum = 0
    while (step * step_y < y_max) {
      sum += {
        if (input(step * step_y)((step * step_x) % x_max) == "#")
          1
        else 0
      }
      step += 1
    }
    sum
  }
  def firstPart(): Int = {
    get_sum(3,1)
  }

  def secondPart(): Int = {
    val step_list: List[(Int, Int)]= List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    step_list.map(x => get_sum(x._1, x._2)).product
  }


  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }
}
