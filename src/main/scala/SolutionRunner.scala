import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution13("input13.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
