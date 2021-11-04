package differentiable.reversemode.chad.onedim

import scala.language.{existentials, implicitConversions}
import scala.math.pow

trait SemiGroup[T]:
  extension (l: T) def +(r: T): T

trait Monoid[T] extends SemiGroup[T]:
  def zero: T

given Monoid[Double] with
  extension (l: Double) def +(r: Double): Double = l + r
  val zero: Double = 0.0


case class Dual(v: Double, d: Double => Double)
extension[DOut <: Double: Monoid] (thisRealDual: Dual)
  def *(thatRealDual: Dual): Dual = mulTwo(thisRealDual, thatRealDual)
  def +(thatRealDual: Dual): Dual = sumTwo(thisRealDual, thatRealDual)


def variable[Val <: Double](v: Val): Dual = Dual(v, identity)

def const(v: Double): Dual = Dual(v, _ => summon[Monoid[Double]].zero)

def mulTwo(l: Dual, r: Dual): Dual =
  def dMul(x: (Double, Double))(y: Double): (Double, Double) =
    (y*x._2, y*x._1)

  def dCombinedInput(dMulResult: (Double, Double)) =
    l.d(dMulResult._1) + r.d(dMulResult._2)

  Dual(
    l.v * r.v,
    y =>
      dCombinedInput(dMul(l.v, r.v)(y))
  )

def sumTwo(l: Dual, r: Dual): Dual =
  def dSum(y: Double): (Double, Double) =
    (y, y)

  def dCombinedInput(dSumResult: (Double, Double)) =
    l.d(dSumResult._1) + r.d(dSumResult._2)

  Dual(
    l.v + r.v,
    y =>
      dCombinedInput(dSum(y))
  )


given Conversion[Double, Dual] = const(_)
given Conversion[Int, Dual] = const(_)

@main def main() =
//  val x = 5.0
//  println(mulOne(variable(5, 4)).d(1.0))
  
  val x = variable(3)
//  val y1 = 2 * x
//  val y2 = x * x
//  val y3 = y2 * x
//  println((y1 + y3).d(1))
  val b = 2 * x
  println((b + (1.0/8.0) * b * b * b).d(1))
