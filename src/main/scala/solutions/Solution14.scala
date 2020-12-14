package solutions

import scala.io.Source

class Solution14(str: String) extends Solution {
  val input = Source.fromResource(s"data/$str").getLines.toArray
  def resolveMask(c: Char, c1: Char) = {
    if (c1 == 'X') c
    else c1
  }
  def calculateValue(currentMask: String, number: Long): Long = {
    val numberBin = number.toBinaryString.reverse.padTo(36, '0').reverse
    val newValue =
      numberBin.zip(currentMask).map(x => resolveMask(x._1, x._2)).mkString("")
    java.lang.Long.parseLong(newValue.toString, 2)
  }

  def firstPart(): Long = {
    val memoryMap: collection.mutable.Map[Int, Long] =
      collection.mutable.Map(0 -> 0)
    val reg = "[0-9]+".r
    var currentMask: String = ""
    val data = input.map(_.split(" = "))
    for (operation <- data) {
      if (operation(0) == "mask") currentMask = operation(1)
      else {
        val mem: Int = reg.findFirstIn(operation(0)).get.toInt
        memoryMap(mem) = calculateValue(currentMask, operation(1).toLong)
      }

    }
    memoryMap.values.sum
  }

  def resolveFloatMask(x: (Char, Char)): Char = {
    if (x._1 == '0') x._2
    else if (x._1 == '1') '1'
    else 'X'
  }

  def substituteFloats(x: String, floatMask: String): String = {
    var index = 0
    var outputString = floatMask.toArray
    for (char <- floatMask.indices) {
      if (floatMask(char) == 'X') {
        outputString(char) = x(index)
        index += 1
      }
    }
    outputString.mkString("")
  }

  def generateAddresses(floatMask: String): Array[Long] = {
    val amountOfFloats: Int = scala.math.pow(2, floatMask.count(_ == 'X')).toInt
    val range = 0 until amountOfFloats
    val subValues = range
      .map(
        _.toBinaryString.reverse
          .padTo(amountOfFloats.toBinaryString.length - 1, '0')
          .reverse)
      .toArray
    subValues
      .map(x => substituteFloats(x, floatMask))
      .map(x => java.lang.Long.parseLong(x.toString, 2))
  }

  def calculateAddresses(currentMask: String, address: Long): Array[Long] = {
    val floatMask = currentMask
      .zip(address.toBinaryString.reverse.padTo(36, '0').reverse)
      .map(x => resolveFloatMask(x))
      .toArray
    generateAddresses(floatMask.mkString(""))
  }

  def secondPart(): Long = {
    val memoryMap: collection.mutable.Map[Long, Long] =
      collection.mutable.Map(0L -> 0L)
    val reg = "[0-9]+".r
    var currentMask: String = ""
    val data = input.map(_.split(" = "))
    for (operation <- data) {
      if (operation(0) == "mask") currentMask = operation(1)
      else {
        val mem: Int = reg.findFirstIn(operation(0)).get.toInt
        val addresses: Array[Long] = calculateAddresses(currentMask, mem.toLong)
        for (address <- addresses) memoryMap(address) = operation(1).toLong
      }

    }
    memoryMap.values.sum
  }

  override def getSolution(): (Long, Long) = (firstPart(), secondPart())

}
