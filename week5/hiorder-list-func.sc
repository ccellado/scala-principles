def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil     => xs
    case y :: ys => y * y :: squareList(ys)
  }

def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => x * x)

def pack[T](xs: List[T]): List[List[T]] =
  xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

def encode[T](xs: List[T]): List[(T, Int)] = {
  val toFold = pack(xs)
  def foldMy[T](mapa: List[T], acc: Int): (T, Int) =
    mapa match {
      case head :: Nil  => (head, acc)
      case head :: tail => foldMy(tail, acc + 1)
    }
  def encodePacked[T](ys: List[List[T]]): List[(T, Int)] = {
    ys match {
      case Nil          => Nil
      case head :: tail => foldMy(head, 1) :: encodePacked(tail)
    }
  }
  encodePacked(toFold)
}
def encodeMap[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (ys => (ys.head, ys.length))

}
println(encodeMap(List("a", "a", "a", "b", "c", "c", "a")))
println(pack(List("a", "a", "a", "b", "c", "c", "a")))

def mapFun[T, U](xs: List[T])(f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, acc) => acc + 1)

val listfun: List[Int] = List(1, 2, 3, 4, 5, 6)

println(lengthFun(listfun))
