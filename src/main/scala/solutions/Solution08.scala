package solutions

import scala.io.Source

class Solution08(inputFile: String) {
  var accumulator = 0
  var instruction = 0
  val input: Array[(String, Int)] =
    Source
      .fromResource(s"data/$inputFile")
      .getLines
      .toArray
      .map(_.split(" "))
      .map(x => (x(0), x(1).toInt))
  def jump(x: Int): Int = {
    instruction + x
  }
  def accumulate(x: Int): Int = {
    accumulator += x
    instruction + 1
  }
  def nothing(): Int = {
    instruction + 1
  }
  def executeRule(instruction: (String, Int)): Int = {
    instruction match {
      case ("jmp", x) => jump(x)
      case ("acc", x) => accumulate(x)
      case ("nop", _) => nothing()
      case _          => -1
    }
  }

  def firstPart(inpt: Array[(String, Int)]): (Int, Boolean) = {
    val doneInstructions: collection.mutable.Set[Int] = collection.mutable.Set()
    val instructions = inpt
    var goodExit = false
    while (!doneInstructions.contains(instruction)) {
      if (instruction < instructions.length) {
        doneInstructions.add(instruction)
        instruction = executeRule(instructions(instruction))
      }
      else if(instruction == instructions.length){
        goodExit = true
        doneInstructions.add(instruction)
      }
    }
    (accumulator, goodExit)
  }

  def changeInput(input: Array[(String, Int)], i: Int): Array[(String, Int)] = {
    val inpt = input.clone
    input(i) match {
      case ("jmp", x) => inpt.update(i, ("nop", x))
      case ("nop", x) => inpt.update(i, ("jmp", x))
      case ("acc", x) =>
    }
    inpt
  }

  def secondPart(): Int = {
    accumulator = 0
    instruction = 0
    var i = 0
    var terminate = false
    var result = 0
    while (!terminate && i < input.length) {
      val fixedInput = changeInput(input, i)
      val (res, term) = firstPart(fixedInput)
      terminate = term
      result = res
      accumulator = 0
      instruction = 0
      i += 1
    }
    result
  }
  def getSolution(): (Int, Int) = {
    (firstPart(input)._1, secondPart())
  }
}
