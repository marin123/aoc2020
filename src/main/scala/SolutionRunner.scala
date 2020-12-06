import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution06("input06.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
