import java.{util => ju}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def toStr: String = {
    if (tail.isEmpty)
      return "(" + head + ")" + "Nil"
    else
      return "(" + head + ")" + tail.toStr
  }
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

def singleton[T](elem: T): Cons[T] = new Cons[T](elem, new Nil[T])
def pair[T](car: T, cdr: List[T]): Cons[T] = new Cons[T](car, cdr)
def nth[T](list: List[T], num: Int): T =
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (num == 0) list.head
  else nth(list.tail, num - 1)

val newlist = pair(1, pair(2, pair(3, pair(4, pair(5, singleton(6))))))

print(nth(newlist, 5))
