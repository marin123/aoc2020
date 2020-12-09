package solutions

import scala.io.Source

class Solution09(inputFile: String) extends Solution {
  val movingWindow = 25
  val input: Array[Long] = Source
    .fromResource(s"data/$inputFile")
    .getLines
    .toArray.map(_.toLong)
  def findAllPossibleSums(window: Array[Long]): Set[Long] = {
    window.flatMap(x => window.map(y => (x,y)).map(x => x._1 + x._2)).toSet
  }

  def firstPart(): Long = {
    var i=0
    var isWrong = false
    while (i < input.length && !isWrong) {
      isWrong = {if (i < movingWindow) false
      else {
        val window = input.slice(i - movingWindow, i)
        val sums = findAllPossibleSums(window)
        if (sums.contains(input(i))) false
        else true}
      }
      i+=1
    }
    input(i-1)
  }
  def secondPart(): Long = {
    val inputNumber = firstPart()
    var start = 0
    var end = 0
    var foundAnswer = false
    while (!foundAnswer){
      val sum = input.slice(start, end).sum
      if (sum < inputNumber) end += 1
      else if (sum > inputNumber){
        start += 1
        end = start
      }
      else if(sum == inputNumber) foundAnswer = true
    }
    input.slice(start, end).max + input.slice(start, end).min
  }

  def getSolution(): (Long, Long) = {
    (firstPart(), secondPart())
  }

}
