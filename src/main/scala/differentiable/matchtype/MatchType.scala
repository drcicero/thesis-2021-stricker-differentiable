package differentiable.matchtype

import differentiable.matchtype.Poly

import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.quoted._

sealed trait Poly:
  def apply(x: Double): Double

@showAsInfix case class *[L <: Poly, R <: Poly](l: L, r: R) extends Poly:
  override def apply(x: Double): Double = l(x) * r(x)
  override def toString: String = s"$l * $r"

@showAsInfix case class +[L <: Poly, R <: Poly](l: L, r: R) extends Poly:
  override def apply(x: Double): Double = l(x) + r(x)
  override def toString: String = s"($l + $r)"

@showAsInfix case class **[B <: Poly, N <: Int](b: B, n: N) extends Poly:
  override def apply(x: Double): Double =
    val bValue = b(x)
    if bValue == 0 then 0 else Math.pow(bValue, n)
  override def toString: String = s"($b)**$n"

case class X() extends Poly:
  override def apply(x: Double): Double = x
  override def toString: String = "X"

case class V[C <: Double | Int](c: C) extends Poly:
  override def apply(x: Double): Double = c match
    case d: Double => d
    case i: Int => i
  override def toString: String = c.toString

type D[P <: Poly] <: Poly = P match
  case l * r => l * D[r] + D[l] * r
  case l + r => D[l] + D[r]
  case X ** n => n match
    case 0 => V[0]
    case 1 => V[1]
    case _ => V[n] * (X**(n-1))
  case X => V[1]
  case V[_] => V[0]

inline def initPolynomial[P <: Poly]: P =
  val res = inline erasedValue[P] match
    case _: (l * r) => new *(initPolynomial[l], initPolynomial[r])
    case _: (l + r) => new +(initPolynomial[l], initPolynomial[r])
    case _: (b ** n) => new **(initPolynomial[b], constValue[n])
    case _: X => X()
    case _: V[c] => V(constValue[c])
  res.asInstanceOf[P]

inline def d[P <: Poly]: D[P] = initPolynomial