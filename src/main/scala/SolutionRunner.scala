import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution17("input17.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
