// def times(chars: List[Char]): List[(Char, Int)] = {
//   def inPair(hay: List[Char], needle: List[(Char, Int)]): List[(Char, Int)] = {
//     def isPresent(n: List[(Char, Int)], )
//     if (needle.length > 1)
//       (hay.head, hay.count(x => x == needle.head)) :: inPair(
//         hay.tail,
//         hay.head :: needle
//       )
//     else isPresent(needle, hay.head)
//   }
//   chars match {
//     case Nil     => Nil
//     case x :: xs => inPair(chars, (chars.head, 1) :: Nil)
//   }
// }

def times(chars: List[Char]): List[(Char, Int)] = {
  def insertChar(chars: List[(Char, Int)], C: Char): List[(Char, Int)] =
    chars match {
      case Nil            => List((C, 1))
      case (C, i) :: tail => (C, i + 1) :: tail
      case head :: tail   => head :: insertChar(tail, C)
    }
  chars match {
    case Nil          => Nil
    case head :: tail => insertChar(times(tail), head)
  }
}

val newlist = 'b' :: 'a' :: 'a' :: 'c' :: Nil
val newlist_w = times(newlist)

val intlist = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

def listSum(list: List[Int]): Int = {
  def fun(list: List[Int], acc: Int): Int =
    list match {
      case Nil          => acc
      case head :: tail => fun(tail, acc + head)
    }
  fun(list, 0)
}

trait Monoid[A] {
  def zero: A
  def combine(x: A, y: A): A
}

implicit val intSumMonoid: Monoid[Int] = new Monoid[Int] {
  def zero: Int = 0
  def combine(x: Int, y: Int): Int = x + y
}

def listSumMon[A](list: List[A], mono: Monoid[A]): A = {
  def fun(list: List[A], acc: A): A =
    list match {
      case Nil          => acc
      case head :: tail => fun(tail, mono.combine(head, acc))
    }
  fun(list, mono.zero)
}

println(listSumMon(intlist, intSumMonoid))
