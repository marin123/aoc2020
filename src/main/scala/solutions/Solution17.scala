package solutions

import scala.io.Source

class Solution17(str: String) extends Solution {
  val input: Seq[Seq[Char]] = Source.fromResource(s"data/$str").getLines().toSeq.map(_.toCharArray)
  def firstPart(): Any = {
    input
    0
  }

  def secondPart(): Any = 0

  def getSolution(): Any = (firstPart(), secondPart())
}
