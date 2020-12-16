package solutions

import scala.io.Source

class Solution16(str: String) extends Solution {
  val input: Array[String] = Source.fromResource(s"data/$str").getLines.toArray
  val rules: Map[String, Array[(Int, Int)]] = input
    .takeWhile(_ != "")
    .map(_.split(": "))
    .map(
      x =>
        x(0) -> x(1)
          .split(" or ")
          .map(y => (y.split("-")(0).toInt, y.split("-")(1).toInt)))
    .toMap
  val myTicket: Array[Int] = input
    .dropWhile(!_.startsWith("your ticket"))
    .take(2)
    .last
    .split(",")
    .map(_.toInt)
  val tickets: Array[Array[Int]] = input
    .dropWhile(!_.startsWith("nearby tickets"))
    .drop(1)
    .map(x => x.split(",").map(_.toInt))
  val allRules: Array[(Int, Int)] = rules.values.flatten.toArray

  def isGood(i: Int, allRules: Map[String, Array[(Int, Int)]]): Boolean = {
    val matchesRules: Int = allRules.values.count(x => (i >= x(0)._1 && i <= x(0)._2) || (i >= x(1)._1 && i <= x(1)._2))
    if (matchesRules > 0) true
    else false
  }

  def firstPart(): Int = {
    tickets.flatten.filter(!isGood(_, rules)).sum
  }

  def checkIfRulesTrue(x: Int, tuples: Array[(Int, Int)]): Boolean = {
    if ((x >= tuples(0)._1 && x <= tuples(0)._2) || (x >= tuples(1)._1 && x <= tuples(1)._2)) true
    else false
  }

  def secondPart(): Long = {
    val badNumbers = tickets.flatten.toSet.diff(tickets.flatten.filter(isGood(_, rules)).toSet)
    val validTickets = tickets.filter(x => badNumbers.intersect(x.toSet).isEmpty)
    val columns: collection.mutable.Map[Int, Array[Int]] = collection.mutable.Map()
    val ruleCandidates: collection.mutable.Map[String, Array[Int]] = collection.mutable.Map()
    val finalRules: collection.mutable.Map[String, Int] = collection.mutable.Map()
    for (i <- validTickets(0).indices){
      val column = validTickets.map{_(i)}
      columns(i) = column
    }
    for (i <- columns.keys){
      for (j <- rules.keys){
        val countFalse = columns(i).map(x => checkIfRulesTrue(x, rules(j))).count(_ == false)
        if (countFalse == 0) {
          if (!ruleCandidates.contains(j)) ruleCandidates(j) = Array(i.toInt)
          else ruleCandidates(j) = ruleCandidates(j) :+ i.toInt
        }
      }
    }
    for (i <- 0 to 20){
      val rules = ruleCandidates.filter(_._2.length <= i)
      for (rule <- rules){
        val columnCandidates = rule._2.toSet.diff(finalRules.values.toSet)
        if (columnCandidates.size == 1) finalRules(rule._1) = columnCandidates.head
      }
    }

    val numbers = Array(myTicket(finalRules("departure location")),
      myTicket(finalRules("departure station")),
      myTicket(finalRules("departure platform")),
      myTicket(finalRules("departure track")),
      myTicket(finalRules("departure date")),
      myTicket(finalRules("departure time"))).map(_.toLong).product
    numbers

  }

  def getSolution(): (Int, Long) = (firstPart(), secondPart())
}
