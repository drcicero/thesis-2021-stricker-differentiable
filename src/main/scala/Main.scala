import scala.quoted._

import compiletime.{constValue, erasedValue}
import compiletime.ops.int._
import scala.annotation.showAsInfix

sealed trait Poly
@showAsInfix case class *[L <: Poly, R <: Poly](l: L, r: R) extends Poly:
  override def toString: String = s"$l * $r"
@showAsInfix case class +[L <: Poly, R <: Poly](l: L, r: R) extends Poly:
  override def toString: String = s"($l + $r)"
@showAsInfix case class ^[B <: Poly, N <: Int](b: B, n: N) extends Poly:
  override def toString: String = s"($b^$n)"
case class X() extends Poly:
  override def toString: String = "X"
case class V[C <: Double | Int](c: C) extends Poly:
  override def toString: String = c.toString
// Add Constant with expression in it

type S[P <: Poly] <: Poly = P match
  case V[0.0] * V[0.0] => V[0.0]
  case V[0.0] * r | l * V[0.0] => V[0.0]
  case V[1.0] * r => S[r]
  case l * V[1.0] => S[l]
  case l * r => S[l] * S[r]

  case V[0.0] + V[0.0] => V[0.0]
  case V[0.0] + r => S[r]
  case l + V[0.0] => S[l]
  case l + r => S[l] + S[r]

  case _ ^ 0 => V[1.0]
  case V[0.0] ^ n => V[0.0]
  case b ^ 1 => S[b]
  case b ^ n => S[b] ^ n

  case X => X
  case V[c] => V[c]


type D[P <: Poly] <: Poly = S[P] match
  case l * r => S[S[l * D[r]] + S[D[l] * r]]
  case l + r => S[D[l] + D[r]]
  case X ^ n => S[V[n] * (X^(n-1))]
  case X => V[1.0]
  case V[_] => V[0.0]


inline def initPolynomial[P <: Poly]: P =
  val res = inline erasedValue[P] match
    case _: (l * r) => new *(initPolynomial[l], initPolynomial[r])
    case _: (l + r) => new +(initPolynomial[l], initPolynomial[r])
    case _: (b ^ n) => new ^(initPolynomial[b], constValue[n])
    case _: X => X()
    case _: V[c] => V(constValue[c])
  res.asInstanceOf[P]

inline def d[P <: Poly]: S[D[S[P]]] = initPolynomial

@main def main: String =
  val x = initPolynomial[D[V[5] * (X^2)]]
  println("hello")
  println(x)
  ""