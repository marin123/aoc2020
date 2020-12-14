import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution14("input14.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
