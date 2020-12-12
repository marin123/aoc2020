import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution11("input11.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
