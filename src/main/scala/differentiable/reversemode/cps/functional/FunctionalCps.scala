package differentiable.reversemode.cps.functional

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

class Num(val x: Double):
  def +(that: Num)(k: Cont): Deriv =
    val y = Num(this.x + that.x)
    val deriv = k(y)
    val derivWithThis = updateDeriv(deriv, this, y){ identity }
    updateDeriv(derivWithThis, that, y){ identity }

  def *(that: Num)(k: Cont): Deriv =
    val y = Num(this.x * that.x)
    val deriv = k(y)
    val derivWithThis = updateDeriv(deriv, this, y){ that.x * _ }
    updateDeriv(derivWithThis, that, y){ this.x * _ }

  private def updateDeriv(deriv: Deriv, key: Num, y: Num)(op: (yd: Double) => Double): Deriv =
    deriv + (key -> (
      deriv(key) + op(deriv(y))
      ))

  override def toString: String = x.toString
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
  // 2x + x^3 => 2 + 3x^2
  println(grad(f)(3))