package solutions
import scala.io.Source

class Solution01(inputFile: String) extends Solution {
  val input: List[Int] = Source.fromResource(s"data/$inputFile").getLines.toList.map(x => x.toInt)
  def firstPart(): Int = {
    val inputInts: List[Int] = input
    val sums: List[(Int, Int, Int)] = inputInts.flatMap(x => inputInts.map(y => (x, y, x + y)))
    val correctSum = sums.filter(_._3 == 2020).head
    println(correctSum)
    correctSum._1 * correctSum._2
  }
  def secondPart(): Int = {
    val inputInts: List[Int] = input
    val sums: List[(Int, Int, Int, Int)] = inputInts.flatMap(x => inputInts.flatMap(y => inputInts.map(z =>(x, y, z, x + y + z))))
    val correctSum = sums.filter(_._4 == 2020).head
    println(correctSum)
    correctSum._1 * correctSum._2 * correctSum._3
  }
  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }
}
