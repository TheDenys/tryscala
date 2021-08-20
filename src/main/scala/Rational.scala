class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  def <(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def >(that: Rational) = this.numer * that.denom > that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  def *(that: Rational) = new Rational(
    this.numer * that.numer,
    this.denom * that.denom
  )

  def /(that: Rational) = new Rational(
    this.numer * that.denom,
    this.denom * that.numer
  )

  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  override def toString(): String = denom match {
    case 1 => numer.toString
    case _ => s"${numer}/${denom}"
  }
}

object Rational {
  implicit def intToRational(n: Int): Rational = Rational(n)

  def apply(n: Int) = new Rational(n)

  def apply(num: Int, denom: Int) = new Rational(num, denom)
}

//val x = new Rational(1, 3)
//val y = new Rational(5, 7)
//val z = new Rational(3, 2)
//println(x + y)
//println(x - y - z)
//println(y + y)
//println(x < y)
//println(x max y)
//println(x * x + y * y)
//println(new Rational(1, 3) + new Rational(1, 3) + new Rational(1, 3))
