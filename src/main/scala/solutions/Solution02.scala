package solutions

import scala.io.Source

case class Passwords(min: Int, max: Int, letter: String, password: String)
class Solution02(inputFile: String) extends Solution {
  val input: List[String] = Source.fromResource(s"data/$inputFile").getLines.toList
  def checkPassword(password: Passwords): Int = {
    val countLetters:Int  = password.password.count(_ == password.letter.charAt(0))
    val isCorrect = {
      if (countLetters >= password.min && countLetters <= password.max)
        1
      else
        0
    }
    isCorrect
  }
  def checkPassword2(passwords: Passwords): Int ={
    def checkPosition(word: String, letter:String, position: Int): Int ={
      val correctPosition = {
        if (word.length < (position - 1))
          0
        else if (word.charAt(position - 1) == letter.charAt(0))
          1
        else
          0
      }
      correctPosition
    }
    val amountOfCorrectPositions = checkPosition(passwords.password, passwords.letter, passwords.min) +
      checkPosition(passwords.password, passwords.letter, passwords.max)
    val isCorrect = {
      if (amountOfCorrectPositions == 1)
        1
      else
        0
    }
    isCorrect
  }
  def firstPart(): Int = {
    val parsedInput:List[(String, String, String, String)] = input.map(_.split(" "))//re
    .map(x => (x(0).split("-"), x(1).replace(":", ""), x(2)))
      .map(x => (x._1(0), x._1(1), x._2, x._3 ))
    val passwords: List[Passwords] = parsedInput.map(x =>
       Passwords(min = x._1.toInt,
                    max = x._2.toInt,
                    letter = x._3,
                    password = x._4)
    )
    passwords.map(checkPassword).sum
  }
  def secondPart(): Int = {
    val parsedInput:List[(String, String, String, String)] = input.map(_.split(" "))//re
      .map(x => (x(0).split("-"), x(1).replace(":", ""), x(2)))
      .map(x => (x._1(0), x._1(1), x._2, x._3 ))
    val passwords: List[Passwords] = parsedInput.map(x =>
       Passwords(min = x._1.toInt,
        max = x._2.toInt,
        letter = x._3,
        password = x._4)
    )
    passwords.map(checkPassword2).sum
  }

  def getSolution(): (Int, Int) = {
    (firstPart(), secondPart())
  }
}
