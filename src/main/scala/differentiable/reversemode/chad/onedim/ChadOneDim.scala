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

def variable(v: Double): Dual = Dual(v, identity)
def const(v: Double): Dual = Dual(v, _ => summon[Monoid[Double]].zero)

case class Dual(v: Double, variableAdjoint: Double => Double):
  def *(that: Dual): Dual =
    def variableAdjointBothSides(partialAdjointL: Double, partialAdjointR: Double) =
      this.variableAdjoint(partialAdjointL) + that.variableAdjoint(partialAdjointR)

    def partialAdjointThis(parentAdjoint: Double) = parentAdjoint * that.v
    def partialAdjointThat(parentAdjoint: Double) = parentAdjoint * this.v

    Dual(
      this.v * that.v,
      parentAdjoint =>
        variableAdjointBothSides(partialAdjointThis(parentAdjoint), partialAdjointThat(parentAdjoint))
    )
  end *

  def +(that: Dual): Dual =
    def dSum(y: Double): (Double, Double) =
      (y, y)

    def dCombinedInput(dSumResult: (Double, Double)) =
      this.variableAdjoint(dSumResult._1) + that.variableAdjoint(dSumResult._2)

    Dual(
      this.v + that.v,
      y =>
        dCombinedInput(dSum(y))
    )


given Conversion[Double, Dual] = const(_)
given Conversion[Int, Dual] = const(_)

@main def main() =
//  val x = 5.0
//  println(mulOne(variable(5, 4)).d(1.0))

  val x = variable(3)

  val y1 = 2 * x
  val y2 = x * x
  val y3 = y2 * x
  println((y1 + y3).variableAdjoint(1))
//  val b = 2 * x
//  println((b + (1.0/8.0) * b * b * b).d(1))
