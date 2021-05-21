import scala.quoted._
import polynomial.Polyinomial._
import polynomial._

case class &[L, R](l: L, r: R)

@main def main: String =
  val x: V[2.2] * V[5.0] = initPolynomial
  val s: V[5.5] * V[6] * (X^5) = d[V[5.5] * (X^6)]
  ""
  