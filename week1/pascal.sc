def pascal(x: Int, y: Int): Int =
    if (y == 0)
        return 1
    else if (x == 0 || x == y )
        return 1
    else
        return pascal(x - 1, y - 1) + pascal(x, y - 1)

println(pascal(1, 4))
println(pascal(2, 4))
println(pascal(3, 4))
