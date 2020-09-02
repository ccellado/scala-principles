def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

println(product(x => x * x)(1, 3))

def fact(n: Int) = product(x => x)(1, n)

println(fact(5))

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(
    a: Int,
    b: Int
): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

println(mapReduce(x => x * x, (x, y) => x * y, 1)(1, 3))
