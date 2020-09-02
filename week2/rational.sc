class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  override def toString = numer + "/" + denom

  def neg = new Rational(numer * -1, denom)

  def sub(that: Rational) = add(that.neg)
}

val x = new Rational(1, 2)
val y = new Rational(3, 4)

println(y.sub(x).toString)
