package differentiable.macros

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.quoted._
import scala.language.implicitConversions

sealed trait Poly:
  def apply(x: Double): Double
  
  def *(r: Poly): Poly = Mul(this, r)
  def +(r: Poly): Poly = Plus(this, r)
  def **(n: Int): Poly  = Pow(this, n)

case class Mul(l: Poly, r: Poly) extends Poly:
  override def apply(x: Double): Double = l(x) * r(x)
  override def toString: String = s"$l * $r"

case class Plus(l: Poly, r: Poly) extends Poly:
  override def apply(x: Double): Double = l(x) + r(x)
  override def toString: String = s"($l + $r)"

case class Pow(b: Poly, n: Int) extends Poly:
  override def apply(x: Double): Double = powOrElseThrow(b(x), n)
  override def toString: String = s"($b)**$n"
  private def powOrElseThrow(b: Double, n: Int): Double =
    val res = pow(b, n)
    if res.isFinite then res else throw IllegalArgumentException()

object X extends Poly:
  override def apply(x: Double): Double = x
  override def toString: String = "X"

case class V(v: Double) extends Poly:
  override def apply(x: Double): Double = v
  override def toString: String = v.toString

given Conversion[Double, V] = V(_)
given Conversion[Int, V] = V(_)

private def dImpl(p: Expr[Poly])(using Quotes): Expr[Poly] = p match
  case '{ ($l: Poly) + ($r: Poly) } =>
    '{ ${ dImpl(l) } + ${ dImpl(r) } }
  case '{ ($l: Poly) * ($r: Poly) } =>
    '{ $l * ${ dImpl(r) } + ${ dImpl(l) } * $r }
  case '{ ($b: Poly) ** $n } => 
    '{ $n * ${ dImpl(b) } * ($b ** ($n - 1))  }
  case '{ X } => '{ 1 }
  case '{ $v: V } => '{ 0 }

inline def d(inline p: Poly): Poly =
  ${dImpl('p)}