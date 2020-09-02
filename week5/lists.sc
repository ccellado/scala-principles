def removeAt[T](n: Int, xs: List[T]): List[T] =
  n match {
    case 0 => xs.tail
    case _ => xs.head :: removeAt(n - 1, xs.tail)
  }
println(removeAt(1, List('a', 'b', 'c', 'd')))

sealed trait RecHuinya[A]
case class ListNode[A](list: List[RecHuinya[A]]) extends RecHuinya[A]
case class ValueNode[A](value: A) extends RecHuinya[A]

def flatten[A](xs: List[RecHuinya[A]]): List[A] = {
  xs match {
    case Nil => Nil
    case ListNode(x :: y) :: tail =>
      flatten(x :: y) ::: flatten(tail)
    case ValueNode(head) :: tail =>
      head :: flatten(tail)
  }
}
println(
  flatten(
    List(
      ListNode(List(ValueNode(1), ValueNode(1))),
      ValueNode(2),
      ListNode(List(ValueNode(3), ListNode(List(ValueNode(5), ValueNode(8)))))
    )
  )
)
