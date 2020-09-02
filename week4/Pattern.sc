case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object Number extends Expr {
  def apply(n: Int) = new Number(n)
}
object Sum extends Expr {
  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
}
object Prod extends Expr {
  def apply(e1: Expr, e2: Expr) = new Prod(e1, e2)
}

trait Expr {
  def eval: Int =
    this match {
      case Number(n)    => n
      case Sum(e1, e2)  => e1.eval + e2.eval
      case Prod(e1, e2) => e1.eval * e2.eval
    }
  def show: String =
    this match {
      case Number(n) => n.toString
      case Prod(Sum(e1, e2), e3) =>
        '(' + Sum(e1, e2).show + ')' + '*' + e2.show
      case Sum(e1, e2)  => e1.show + '+' + e2.show
      case Prod(e1, e2) => e1.show + '*' + e2.show
    }
}

print(Prod(Sum(Number(1), Number(6)), Number(1)).show)
