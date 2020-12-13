package solutions

import scala.io.Source

class Solution12(str: String) extends Solution {
  val input: Array[(Char, Int)] = Source
    .fromResource(s"data/$str")
    .getLines
    .toArray
    .map(x => (x(0), x.drop(1).toInt))

  def changePosition(instruction: (Char, Int),
                     position: (Int, Int)): (Int, Int) = {
    instruction._1 match {
      case 'N' => (position._1, position._2 + instruction._2)
      case 'S' => (position._1, position._2 - instruction._2)
      case 'E' => (position._1 - instruction._2, position._2)
      case 'W' => (position._1 + instruction._2, position._2)
    }
  }

  def dirToDeg(shipDirection: Char): Int = {
    shipDirection match {
      case 'W' => 0
      case 'N' => 90
      case 'E' => 180
      case 'S' => 270
    }
  }

  def degToDir(shipDirectionDeg: Int): Char = {
    shipDirectionDeg match {
      case 0   => 'W'
      case 90  => 'N'
      case 180 => 'E'
      case 270 => 'S'
    }
  }

  def normalize(newShipDir: Int): Int = {
    if (newShipDir > 270) newShipDir - 360
    else if (newShipDir < 0) newShipDir + 360
    else newShipDir
  }

  def changeDirection(instruction: (Char, Int), shipDirection: Char): Char = {
    val shipDirDeg: Int = dirToDeg(shipDirection)
    val newShipDir: Int = {
      if (instruction._1 == 'R') shipDirDeg + instruction._2
      else if (instruction._1 == 'L') shipDirDeg - instruction._2
      else 0
    }
    degToDir(normalize(newShipDir))
  }

  override def firstPart(): Int = {
    var shipDirection: Char = 'E'
    var shipPosition: (Int, Int) = (0, 0)
    for (instruction <- input) {
      if (Set('N', 'S', 'E', 'W').contains(instruction._1))
        shipPosition = changePosition(instruction, shipPosition)
      else if (Set('L', 'R').contains(instruction._1))
        shipDirection = changeDirection(instruction, shipDirection)
      else if (instruction._1 == 'F')
        shipPosition =
          changePosition((shipDirection, instruction._2), shipPosition)
    }
    println(shipPosition)
    shipPosition._1.abs + shipPosition._2.abs
  }

  def rotateWaypoint(instruction: (Char, Int), waypointPosition: (Int, Int)): (Int, Int) = {
    var newWaypoint = (0, 0)
    if (instruction._1 == 'R'){
      newWaypoint = instruction._2 match {
        case 0 => waypointPosition
        case 90 => (-waypointPosition._2, waypointPosition._1)
        case 180 => (-waypointPosition._1, -waypointPosition._2)
        case 270 => (waypointPosition._2, -waypointPosition._1)
      }
    }
    else {
      newWaypoint = instruction._2 match {
        case 0 => waypointPosition
        case 90 => (waypointPosition._2, -waypointPosition._1)
        case 180 => (-waypointPosition._1, -waypointPosition._2)
        case 270 => (-waypointPosition._2, waypointPosition._1)
      }
    }
    newWaypoint
  }

  def goTowardsEndpoint(waypointPosition: (Int, Int), times: Int, shipPosition: (Int, Int)): (Int, Int) = {
    (shipPosition._1 + times * waypointPosition._1, shipPosition._2 + times * waypointPosition._2 )
  }

  override def secondPart(): Int = {
    var waypointPosition = (-10, 1)
    var shipPosition = (0, 0)
    for (instruction <- input) {
      if (Set('N', 'S', 'E', 'W').contains(instruction._1))
        waypointPosition = changePosition(instruction, waypointPosition)
      else if (Set('L', 'R').contains(instruction._1))
        waypointPosition = rotateWaypoint(instruction, waypointPosition)
      else if (instruction._1 == 'F')
        shipPosition =
          goTowardsEndpoint(waypointPosition, instruction._2, shipPosition)
      println(waypointPosition, shipPosition)
    }
    println(shipPosition)
    shipPosition._1.abs + shipPosition._2.abs
  }
  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }

}
