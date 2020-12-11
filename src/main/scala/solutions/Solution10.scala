package solutions

import scala.io.Source

class Solution10(inputFile: String) extends Solution {
  val input: Array[Int] = Source
    .fromResource(s"data/$inputFile")
    .getLines
    .toArray
    .map(_.toInt)
  override def firstPart(): Int = {
    var currenValue = 0
    val diffs = collection.mutable.Map(1 -> 0, 2 -> 0, 3 -> 0)
    for (x <- input.sorted if x!= 0) {
      val diff = x - currenValue
      diffs.update(diff, diffs(diff) + 1)
      currenValue = x
    }
    diffs.update(3, diffs(3) + 1)
    diffs(1) * diffs(3)
  }

  def createLinkMap(input: Array[Int]): Map[Int, Seq[Int]] = {
    val emptyIntArray: Seq[Int] = Seq()
    val maxElement: Map[Int, Seq[Int]] = Map(input.max -> emptyIntArray)
    val linkMap = input
      .flatMap(x => input.map(y => (x, y)))
      .filter(x => x._2 - x._1 <= 3)
      .filter(x => x._2 - x._1 >= 1)
      .groupBy(_._1)
      .map(x => x._1 -> x._2.map(y => y._2).toSeq)
    linkMap.++(maxElement)
  }

  def searchPaths[T](start: T, max: T, graph: Map[T, Seq[T]]): Long = {
    var count: Long = 0
    val stack = scala.collection.mutable.Stack(start -> List(start))
    while (stack.nonEmpty) {
      val (current, path) = stack.pop
      for (next <- graph(current)) {
        val newPath = next :: path
        if (newPath.contains(max)) {
          count += 1
        }
        stack.push((next, newPath))
      }
    }
    count
  }

  def getBottlenecks(input: Array[Int]): Array[Int] = {
    var bottleneckArray = Array(0)
    var i = 0
    while (i < input.length) {

      if (i > 0 && i < input.length - 1) {
        val diff_next = input(i + 1) - input(i)
        val diff_prev = input(i) - input(i - 1)
        if (diff_next == 3 && diff_prev == 3)
          bottleneckArray = bottleneckArray :+ input(i)
      }
      else if (input(i) == input.max) bottleneckArray = bottleneckArray :+ input(i)
      i += 1
    }
    bottleneckArray
  }

  override def secondPart(): Long = {
    val linkMap = createLinkMap(input)
    val bottlenecks: Array[(Int, Int)] =
      getBottlenecks(input.sorted).sliding(2).map(x => (x(0), x(1))).toArray
    var arrayResults: Array[Long] = Array()
    for ( i <- bottlenecks){
      val filteredMap = linkMap.filter(x => i._1 to i._2 contains x._1) ++ Map(i._2 -> Seq())

      val result = searchPaths(i._1, i._2, filteredMap)
      arrayResults = arrayResults :+ result
    }
    arrayResults.reduce(_ * _)
  }

  def getSolution(): (Int, Long) = {
    (firstPart(), secondPart())
  }

}
