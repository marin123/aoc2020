import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution12("input12.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
