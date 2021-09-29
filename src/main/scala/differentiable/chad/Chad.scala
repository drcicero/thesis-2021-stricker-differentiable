package differentiable.chad

import scala.language.implicitConversions

type Real = Double
type Real2 = (Double, Double)
type RealN = Real | Real2

case class Dual[Val <: RealN, DIn <: RealN](v: Val, d: DIn => Val)

def variable[Val <: RealN](v: Val): Dual[Val, Val] = Dual(v, identity)

def const(v: Real): Dual[Real, Real] = Dual(v, _ => 0.0)
def const(v: Real2): Dual[Real2, Real2] = Dual(v, _ => (0.0, 0.0))

def mulOne[DIn <: RealN](t: Dual[Real2, DIn]): Dual[Real, DIn] =
  def dMul(x: Real2)(xD: Real2) = (x, xD) match
    case ((xL, xR), (xDL, xDR)) => xL * xDR + xR * xDL

  t match
    case Dual(tV @ (tL, tR), tD) =>
      Dual(
        tL * tR,
        y =>
          dMul(tV)(tD(y))
      )

def mulTwo[DIn <: RealN](l: Dual[Real, DIn], r: Dual[Real, DIn]): Dual[Real, DIn] =
  def dMul(l: Real, r: Real)(lD: Real, rD: Real) = l * rD + r * lD

  Dual(
    l.v * r.v,
    y =>
      dMul(l.v, r.v)(l.d(y), r.d(y))
  )

def sumTwo[DIn <: RealN](l: Dual[Real, DIn], r: Dual[Real, DIn]): Dual[Real, DIn] =
  Dual(
    l.v + r.v,
    y =>
      l.d(y) + r.d(y)
  )


given Conversion[Real2, Dual[Real2, Real2]] = const(_)
given Conversion[Real, Dual[Real, Real]] = const(_)
given Conversion[Int, Dual[Real, Real]] = const(_)

@main def main() =
//  val x = 5.0
//  println(mulOne(2.0, x).d(0.0, 1.0))

  val x = variable(3)
  val y1 = mulTwo(2, x)
  val y2 = mulTwo(x, x)
  val y3 = mulTwo(y2, x)
  println(sumTwo(y1, y3).d(1))
