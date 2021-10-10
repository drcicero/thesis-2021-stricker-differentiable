package differentiable.reversemode.chad

import scala.language.{existentials, implicitConversions}
import scala.math.pow

type Real = Double
type Real2 = (Double, Double)
type RealN = Real | Real2

trait SemiGroup[T]:
  extension (l: T) def +(r: T): T

trait Monoid[T] extends SemiGroup[T]:
  def zero: T

given Monoid[Real2] with
  extension (l: Real2) def +(r: Real2): Real2 = (l._1 + r._1, l._2 + r._2)
  val zero: Real2 = (0.0, 0.0)

given Monoid[Real] with
  extension (l: Real) def +(r: Real): Real = l + r
  val zero: Real = 0.0


case class Dual[Val <: RealN, DOut <: RealN](v: Val, d: Val => DOut)
extension[DOut <: RealN: Monoid](thisRealDual: Dual[Real, DOut])
  def *(thatRealDual: Dual[Real, DOut]): Dual[Real , DOut] = mulTwo(thisRealDual, thatRealDual)
  def +(thatRealDual: Dual[Real, DOut]): Dual[Real , DOut] = sumTwo(thisRealDual, thatRealDual)


def variable[Val <: RealN](v: Val): Dual[Val, Val] = Dual(v, identity)

def const[Val <: RealN: Monoid](v: Val): Dual[Val, Val] = Dual(v, _ => summon[Monoid[Val]].zero)

def mulOne[DOut <: RealN](t: Dual[Real2, DOut]): Dual[Real, DOut] =
  def dMul(x: Real2)(y: Real) = x match
    case (l, r) => (y*r, y*l)

  t match
    case Dual(tV @ (tL, tR), tD) =>
      Dual(
        tL * tR,
        y =>
          tD(dMul(tV)(y))
      )

def mulTwo[DOut <: RealN: Monoid](
                                                 l: Dual[Real, DOut],
                                                 r: Dual[Real, DOut]
                                               ): Dual[Real , DOut] =
  def dMul(x: Real2)(y: Real): Real2 =
    (y*x._2, y*x._1)

  def dCombinedInput(dMulResult: Real2) =
    l.d(dMulResult._1) + r.d(dMulResult._2)

  Dual(
    l.v * r.v,
    y =>
      dCombinedInput(dMul(l.v, r.v)(y))
  )

def sumTwo[DOut <: RealN: Monoid](l: Dual[Real, DOut], r: Dual[Real, DOut]): Dual[Real, DOut] =
  def dSum(y: Real): Real2 =
    (y, y)

  def dCombinedInput(dSumResult: Real2) =
    l.d(dSumResult._1) + r.d(dSumResult._2)

  Dual(
    l.v + r.v,
    y =>
      dCombinedInput(dSum(y))
  )


given Conversion[Real2, Dual[Real2, Real2]] = const(_)
given Conversion[Real, Dual[Real,  Real]] = const(_)
given Conversion[Int, Dual[Real, Real]] = const(_)

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
