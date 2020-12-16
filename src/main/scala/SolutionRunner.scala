import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution16("input16.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
