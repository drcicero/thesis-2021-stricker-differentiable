import scala.quoted._
import Poly._
import Macros.checkSimplePolynomial

enum Poly:
  def +(r: Poly) = Plus(this, r)
  def *(r: Poly) = Mul(this, r)
  def ^(n: Int) = Pow(this, n)

  case V(v: Double)
  case Plus(l: Poly, r: Poly)
  case Mul(l: Poly, r: Poly)
  case Pow(x: Poly, n: Int)
  case X

@main def main: Unit =
  checkSimplePolynomial(
    V(5) + V(1)*X + (X^5) + V(-6.5)*(X^(5)) + (X^2) + X
  )

  checkSimplePolynomial(
    X^0
  )
  checkSimplePolynomial(
    X^(-1)
  )
  checkSimplePolynomial(
    (X^5) * V(1)
  )
  val notKnownAtCompileTime = 5
  checkSimplePolynomial(
    X^(notKnownAtCompileTime)
  )
  checkSimplePolynomial(
    X * X
  )
  checkSimplePolynomial(
    V(5)^5
  )
  