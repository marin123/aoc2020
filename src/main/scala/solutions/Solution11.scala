package solutions

import scala.io.Source

class Solution11(inputFile: String) extends Solution {
  val input: Array[Array[Char]] = Source
    .fromResource(s"data/$inputFile")
    .getLines
    .toArray
    .map(_.toArray)
  def newState(array: Array[Array[Char]], i: Int, j: Int): Char = {
    def noOccupied(): Int = {
      val neighbourStates = Array(array(i + 1)(j),
                                  array(i - 1)(j),
                                  array(i)(j - 1),
                                  array(i)(j + 1),
                                  array(i + 1)(j + 1),
                                  array(i + 1)(j - 1),
                                  array(i - 1)(j - 1),
                                  array(i - 1)(j + 1))
      neighbourStates.count(_ == '#')
    }
    def noOccupiedTwo(): Int = {
      def getFirstUpRight(): Char = {
        var x = i - 1
        var y = j + 1
        var result = '.'
        while (x > 0 && y > 0 && x < array.length && y < array(0).length && result == '.'){
          if (Set('#', 'L').contains(array(x)(y))) result=array(x)(y)
          x -= 1
          y += 1
        }
        result
      }
      def getFirstDownRight(): Char = {
        var x = i + 1
        var y = j + 1
        var result = '.'
        while (x > 0 && y > 0 && x < array.length && y < array(0).length && result == '.'){
          if (Set('#', 'L').contains(array(x)(y))) result=array(x)(y)
          x += 1
          y += 1
        }
        result
      }
      def getFirstUpLeft(): Char = {
        var x = i - 1
        var y = j - 1
        var result = '.'
        while (x > 0 && y > 0 && x < array.length && y < array(0).length && result == '.'){
          if (Set('#', 'L').contains(array(x)(y))) result=array(x)(y)
          x -= 1
          y -= 1
        }
        result
      }
      def getFirstDownLeft(): Char = {
        var x = i + 1
        var y = j - 1
        var result = '.'
        while (x > 0 && y > 0 && x < array.length && y < array(0).length && result == '.'){
          if (Set('#', 'L').contains(array(x)(y))) result=array(x)(y)
          x += 1
          y -= 1
        }
        result
      }
      val neighbourStates = Array(
        ('.' +: array(i).take(j).filter(Set('#', 'L').contains)).last,
        (array(i).drop(j+1).filter(Set('#', 'L').contains) :+ '.').head,
        ('.' +: array.map{_(j)}.take(i).filter(Set('#', 'L').contains)).last,
        (array.map{_(j)}.drop(i+1).filter(Set('#', 'L').contains) :+ '.').head,
        getFirstUpRight(),
        getFirstDownRight(),
        getFirstUpLeft(),
        getFirstDownLeft()
      )
      neighbourStates.count(_ == '#')
    }
    if (array(i)(j) == 'L' && noOccupiedTwo == 0) '#'
    else if (array(i)(j) == '#' && noOccupiedTwo >= 5) 'L'
    else array(i)(j)
  }
  def generateNewState(array: Array[Array[Char]]): Array[Array[Char]] = {
    var newArray = array.map(_.clone)
    for (i <- 1 until array.length - 1) {
      for (j <- 1 until array(0).length - 1) {
        newArray(i)(j) = newState(array, i, j)
      }
    }
    newArray
  }

  def isConfigSame(newState: Array[Array[Char]], currentState: Array[Array[Char]]): Boolean = {
    var same = true
    for (i <- newState.indices){
      if (!newState(i).sameElements(currentState(i))) same=false
    }
    same
  }

  override def firstPart(): Int = {
    var change = true
    var currentState = input.map(_.clone)
    var newState = input.map(_.clone)
    while (change) {
      newState = generateNewState(currentState).map(_.clone)
      if (isConfigSame(newState, currentState)) change = false
      currentState = newState.map(_.clone)
      println(newState.map(_.mkString(" ")).mkString("\n"))
      println(newState.map(_.count(_ == '#')).sum)
    }
    newState.map(_.count(_ == '#')).sum
  }

  override def secondPart(): Int = {
    0
  }

  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }
}
