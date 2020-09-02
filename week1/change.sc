def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
        return 1
    else if (money > 0 && !coins.isEmpty)
        return countChange(money-coins.head, coins) + countChange(money, coins.tail)
    else
        return 0
}

val coins = List(1, 2)
println((countChange(4, coins)))