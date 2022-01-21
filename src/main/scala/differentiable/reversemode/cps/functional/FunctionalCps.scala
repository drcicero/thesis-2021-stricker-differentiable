package differentiable.reversemode.cps.functional

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

class Num(val x: Double):
  def +(that: Num)(k: Continuation): Adjoints =
    val parent = Num(this.x + that.x)

    def updateDeriv(thisOrThat: Num, derivativeWrtThisOrThat: Double, adjoints: Adjoints) =
      val partialAdjoint = adjoints(parent) * derivativeWrtThisOrThat
      val addedPartialAdjoint = adjoints(thisOrThat) + partialAdjoint
      adjoints + (thisOrThat -> addedPartialAdjoint)

    val adjointsWithParent = k(parent)
    val adjointsWithThis = updateDeriv(this, 1, adjointsWithParent)
    val adjointsWithThat = updateDeriv(that, 1, adjointsWithThis)
    adjointsWithThat

  def *(that: Num)(k: Continuation): Adjoints =
    val parent = Num(this.x * that.x)

    def updateDeriv(thisOrThat: Num, derivativeWrtThisOrThat: Double, adjoints: Adjoints) =
      val partialAdjoint = adjoints(parent) * derivativeWrtThisOrThat
      val addedPartialAdjoint = adjoints(thisOrThat) + partialAdjoint
      adjoints + (thisOrThat -> addedPartialAdjoint)

    val adjointsWithParent = k(parent)
    val adjointsWithThis = updateDeriv(this, that.x, adjointsWithParent)
    val adjointsWithThat = updateDeriv(that, this.x, adjointsWithThis)
    adjointsWithThat

  override def toString: String = x.toString
end Num

given Conversion[Double, Num] = Num(_)
given Conversion[Int, Num] = Num(_)

def grad(f: Num => Continuation => Adjoints)(x: Double): Double =
  val xNum = Num(x)
  val allAdjoints = f(xNum) { (topExpression: Num) =>
    Map(topExpression -> 1.0).withDefaultValue(0)
  }
  allAdjoints(xNum)

type Adjoints = Map[Num, Double]
type Continuation = Num => Adjoints

@main def main() =
  def f(x: Num)(k: Continuation): Adjoints =
    (2 * x) { y1 =>
      (x * x) { y2 =>
        (y2 * x) { y3 =>
          (y1 + y3) {
            k(_)
          }
        }
      }
    }

  val derivative = grad(f)(3)
  // 2x + x^3 => 2 + 3x^2
  println(grad(f)(3))