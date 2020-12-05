package solutions
import scala.io.Source

class Solution04(inputFile: String) extends Solution {
  val input: Array[Map[String, String]] =
    Source
      .fromResource(s"data/$inputFile")
      .getLines()
      .mkString("\n")
      .split("\n\n")
      .map(_.replace('\n', ' '))
      .map(
        _.split(" ")
          .map(x => x.split(":"))
          .map(y => y(0) -> y(1))
          .toMap)
  val keysToCheck = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def checkForKeys(map: Map[String, String]): Int = {
    val isIdCorrect = {
      if (keysToCheck.subsetOf(map.keys.toSet)) 1
      else 0
    }
    isIdCorrect
  }

  def checkByr(byr: String): Boolean = {
    byr.toInt >= 1920 && byr.toInt <= 2002
  }

  def checkIyr(iyr: String): Boolean = {
    iyr.toInt >= 2010 && iyr.toInt <= 2020
  }

  def checkEyr(eyr: String): Boolean = {
    eyr.toInt >= 2020 && eyr.toInt <= 2030
  }

  def checkHgt(hgt: String): Boolean = {
    val hgtUnit = hgt.takeRight(2)
    if (hgtUnit == "cm") {
      val hgtSize = hgt.substring(0, hgt.length - 2).toInt
      hgtSize >= 150 && hgtSize <= 193
    } else if (hgtUnit == "in") {
      val hgtSize = hgt.substring(0, hgt.length - 2).toInt
      hgtSize >= 59 && hgtSize <= 76
    } else false
  }

  def checkHcl(hcl: String): Boolean = {
    val reg = "^#[a-f0-9]{6}$"
    hcl.matches(reg)
  }

  def checkEcl(ecl: String): Boolean = {
    val allowedEcl: Set[String] =
      Set("amb", "blu", "gry", "brn", "grn", "hzl", "oth")
      Set(ecl).subsetOf(allowedEcl)
  }

  def checkPid(pid: String): Boolean = {
    val reg = "^[0-9]{9}$"
    pid.matches(reg)
  }
  def checkForMapValues(value: Map[String, String]): Int = {
    val isValid = {
      if (checkForKeys(value) == 1) {
        if (checkByr(value("byr"))
            && checkEcl(value("ecl"))
            && checkEyr(value("eyr"))
            && checkHcl(value("hcl"))
            && checkHgt(value("hgt"))
            && checkIyr(value("iyr"))
            && checkPid(value("pid"))) 1
        else 0
      } else 0
    }
    isValid
  }

  def firstPart(): Int = {
    input.map(checkForKeys).sum
  }

  def secondPart(): Int = {
    input.map(checkForMapValues).sum
  }

  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }
}
