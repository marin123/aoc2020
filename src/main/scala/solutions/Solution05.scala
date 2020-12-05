package solutions

import scala.io.Source

class Solution05(inputFile: String) extends Solution {
  val input: Array[String] =
    Source.fromResource(s"data/$inputFile").getLines.toArray
  def getRowColumn(string: String): (Int, Int) = {
    val row = string.substring(0, 7).replace('B', '1').replace('F', '0')
    val column = string.takeRight(3).replace('R', '1').replace('L', '0')
    (Integer.parseInt(row, 2), Integer.parseInt(column, 2))
  }
  override def firstPart(): Int = {
    val ids = input.map(getRowColumn).map(x => x._1 * 8 + x._2)
    ids.max
  }

  override def secondPart(): Int = {
    val availableSeats = Set(0,1,2,3,4,5,6,7)
    val rowSeats = input.map(getRowColumn).groupBy(_._1).filter(_._2.length == 7)
    val row: Int = rowSeats.keys.head
    val seats: Set[Int] = rowSeats.values.head.toList.map(x => x._2).toSet
    val theSeat = availableSeats.diff(seats).head
    row * 8 + theSeat
  }

  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }
}
