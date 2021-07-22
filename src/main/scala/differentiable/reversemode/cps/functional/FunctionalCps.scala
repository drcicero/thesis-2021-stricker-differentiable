package differentiable.reversemode.cps.functional

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

class Num(val x: Double):
  def +(that: Num)(k: Cont): Deriv =
    val y = Num(x + that.x)
    val deriv = k(y)
    deriv
      .updated(this, deriv(this) + deriv(y))
      .updated(that, deriv(that) + deriv(y))

  def *(that: Num)(k: Cont): Deriv =
    val y = Num(x * that.x)
    val deriv = k(y)
    deriv
      .updated(this, deriv(this) + that.x * deriv(y))
      .updated(that, deriv(that) + x * deriv(y))
end Num

given Conversion[Double, Num] = Num(_)
given Conversion[Int, Num] = Num(_)

def grad(f: Num => Cont => Deriv)(x: Double): Double =
  val xDual = Num(x)
  val d = f(xDual) { (topExpression: Num) =>
    Map(topExpression -> 1.0).withDefaultValue(0)
  }
  d(xDual)

type Deriv = Map[Num, Double]
type Cont = Num => Deriv

@main def main() =
  def f(x: Num)(k: Cont): Deriv =
    (2 * x) { y1 =>
      (x * x) { y2 =>
        (y2 * x) { y3 =>
          (y1 + y3) {
            k(_)
          }
        }
      }
    }

  println(grad(f)(3))