def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum

println(scalarProduct(List(1.0, 2.0, 3.0), List(3.0, 2.0, 1.0)))
def queens(n: Int) = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }
  placeQueens(n)
}
def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensOnRow = (row - 1 to 0 by -1) zip queens
  queensOnRow forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector
        .fill(queens.length)("* ")
        .updated(col, "Q ")
        .mkString
  "\n" + (lines mkString "\n")
}

println((queens(4) map show) mkString "\n")
