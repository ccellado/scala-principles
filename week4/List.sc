trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def toStr: String = {
    if (tail.isEmpty)
      return "(" + head + ")" + "Nil\n"
    else
      return "(" + head + ")" + tail.toStr
  }
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object List {
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, Nil))
  def apple[T]() = Nil
}

val newlist = new Cons(1, new Cons(2, Nil))

print(List(1, 2).prepend(3).toStr)
