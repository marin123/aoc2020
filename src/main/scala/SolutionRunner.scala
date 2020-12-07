import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution07("input07.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
