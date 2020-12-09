import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution09("input09.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
