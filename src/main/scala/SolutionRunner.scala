import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution08("input08.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
