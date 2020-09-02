package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (r == 0) 1
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(chars: List[Char], b: Int): Boolean = {
      if (b == 0 && chars.isEmpty) true
      else if (b < 0 || (chars.isEmpty && b > 0)) false
      else if (chars.head == '(') isBalanced(chars.tail, b + 1)
      else if (chars.head == ')') isBalanced(chars.tail, b - 1)
      else isBalanced(chars.tail, b)
    }
    isBalanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(
        money,
        coins.tail
      )
    else 0
}
