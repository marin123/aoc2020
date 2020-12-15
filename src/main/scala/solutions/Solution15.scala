package solutions

import scala.io.Source

case class Solution15(str: String) extends Solution {
  val input: Array[Int] = Source.fromResource(s"data/$str").getLines().map(_.split(",")).toArray.head.map(_.toInt)
  def firstPart(): Int = {
    var i = 0
    var prevNumber = 0
    val stackInput = scala.collection.mutable.Stack[Int]()
    stackInput.pushAll(input.reverse)
    val saidNumbers: collection.mutable.Map[Int, (Int, Int)] = collection.mutable.Map()
    for (i <- 0 until 30000000){
      val nextNumber: Int = {
        if (stackInput.nonEmpty){
          val stackNum = stackInput.pop()
          saidNumbers(stackNum) = (i, i)
          stackNum
        }
        else if (saidNumbers.contains(prevNumber)) {
          val ageNum = {
            if(saidNumbers(prevNumber)._2 - saidNumbers(prevNumber)._1  == 0) {
              saidNumbers(0) = ( i , saidNumbers(0)._1)
              0
            }
            else {
              val diff: Int = saidNumbers(prevNumber)._1 - saidNumbers(prevNumber)._2
              saidNumbers(diff) = {if (!saidNumbers.contains(diff)) ( i , i)
              else (i, saidNumbers(diff)._1)}
              diff
            }
          }
          ageNum
        }
        else 0
      }
      prevNumber = nextNumber
    }
    prevNumber
  }

  def secondPart(): Int = {
    0
  }

  def getSolution(): (Int, Int) = (firstPart(), secondPart())
}
