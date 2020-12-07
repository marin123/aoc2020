package solutions

import scala.io.Source

class Solution07(inputFile: String) extends Solution {
  val input: Array[String] =
    Source
      .fromResource(s"data/$inputFile")
      .getLines
      .toArray
  val bagMaps: Map[String, Array[(Option[Int], Option[String])]]
  = input.map(_.replace("bags", "bag"))
    .map(_.split("contain"))
    .map(
      x =>
        x(0).split(" ").take(2).mkString(" ") -> x(1).split(",").map(convertRuleToTuple))
    .toMap
  def convertRuleToTuple(rule: String): (Option[Int], Option[String]) = {
    val parsedRule = rule.trim.split(" ").toArray
    val tuple: (Option[Int], Option[String]) = parsedRule match {
      case Array("no", "other", "bag.") => (None, None)
      case Array("") => (None, None)
      case _ => (Option(parsedRule(0).toInt), Option(s"${parsedRule(1)} ${parsedRule(2)}"))
    }
    tuple
  }

  def containsBags(bag: String): Set[String]= {
    val seenBags = collection.mutable.Set(bag)
    val queue = collection.mutable.ArrayDeque(bag)
    while (queue.nonEmpty) {
      val current = queue.removeHead()
      for (next <- bagMaps(current).map(x => x._2) if !seenBags.contains(next.getOrElse("")) && next.isDefined){
        seenBags.add(next.get)
        queue.append(next.get)
      }
    }
    seenBags.toSet
  }

  def containsAmountOfBags(bag: String): Int= {
    var amountOfBags = 0
    val queue = collection.mutable.ArrayDeque((bag, 1))
    while (queue.nonEmpty) {
      val current = queue.removeHead()
      for ((number, next) <- bagMaps(current._1) if next.isDefined){
        amountOfBags += number.get * current._2
        queue.append((next.get, number.get * current._2))
      }
    }
    amountOfBags
  }

  def firstPart(): Int = {
    val bagContents = bagMaps.map(x => (x, containsBags(x._1)))
    bagContents.count(_._2.contains("shiny gold")) - 1 //to exclude the shiny gold itself
  }
  def secondPart(): Int = {
    containsAmountOfBags("shiny gold")
  }

  def getSolution(): (Any, Int) = {
    (firstPart(), secondPart())
  }
}
