package differentiable.dualnumber

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.quoted._

sealed trait Poly:
  def apply(x: Double): Double
  def d(x: Double): Double

  def *(r: Poly): Poly = Mul(this, r)
  def +(r: Poly): Poly = Plus(this, r)
  def **(n: Int): Poly  = Pow(this, n)

case class Mul(l: Poly, r: Poly) extends Poly:
  override def apply(x: Double): Double = l(x) * r(x)
  override def d(x: Double): Double =
    l(x) * r.d(x) + l.d(x) * r(x)
  override def toString: String = s"$l * $r"

case class Plus(l: Poly, r: Poly) extends Poly:
  override def apply(x: Double): Double = l(x) + r(x)
  override def d(x: Double): Double = l.d(x) + r.d(x)
  override def toString: String = s"($l + $r)"

case class Pow(b: Poly, n: Int) extends Poly:
  override def apply(x: Double): Double = powOrElseThrow(b(x), n)
  override def d(x: Double): Double = n match
    case 0 => 0
    case 1 => b.d(x)
    case _ => n * b.d(x) * powOrElseThrow(b(x), n - 1)
  override def toString: String = s"($b)**$n"
  private def powOrElseThrow(b: Double, n: Int): Double =
    val res = pow(b, n)
    if res.isFinite then res else throw IllegalArgumentException()

object X extends Poly:
  override def apply(x: Double): Double = x
  override def d(x: Double): Double = 1
  override def toString: String = "X"

case class V(v: Double) extends Poly:
  override def apply(x: Double): Double = v
  override def d(x: Double): Double = 0
  override def toString: String = v.toString

given Conversion[Double, V] = V(_)
given Conversion[Int, V] = V(_)

@main def main() =
  import scala.language.implicitConversions
  val f = 6 * 3 * (X**5) + 5
  println(0 * -0.1)