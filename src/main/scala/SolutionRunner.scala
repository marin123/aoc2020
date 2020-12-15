import solutions._
object SolutionRunner {
  def main(args: Array[String]): Unit = {
    val solution = new Solution15("input15.txt")
    println(s"Solution is ${solution.getSolution()}")
  }
}
