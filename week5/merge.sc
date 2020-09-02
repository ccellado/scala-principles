def msort(xs: List[Int]): List[Int] = {
  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (h1 :: t1, h2 :: t2) =>
        if (h1 < h2) h1 :: merge(t1, ys)
        else h2 :: merge(t2, xs)
    }
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val sortme = 2 :: 9 :: 0 :: 1 :: 5 :: 1 :: 11 :: 3 :: Nil

println(msort(sortme))
