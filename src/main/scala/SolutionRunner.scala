import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution05("input05.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
