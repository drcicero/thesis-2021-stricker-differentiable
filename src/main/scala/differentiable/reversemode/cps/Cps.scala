package differentiable.reversemode.cps

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

case class Dual(x: Double, var d: Double):
  def +(r: Dual)(k: Dual => Dual): Dual =
    val y = Dual(x + r.x, 0)
    val res = k(y)
    d += y.d
    r.d += y.d
    res

  def *(r: Dual)(k: Dual => Dual): Dual =
    val y = Dual(x * r.x, 0)
    val res = k(y)
    d += r.x * y.d
    r.d += x * y.d
    res

given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def grad(f: Dual => (Dual => Dual) => Dual)(x: Double): Double = {
  val xDual = Dual(x, 0)
  f(xDual) { topExpression => {
    topExpression.d = 1
    topExpression
  }
  }
  xDual.d
}


@main def main() =
  def f(x: Dual)(k: Dual => Dual): Dual =
    (2 * x) { y1 =>
      (x * x) { y2 =>
        (y2 * x) { y3 =>
          (y1 + y3) {
            k(_)
          }
        }
      }
    }

  println(f(3){ identity(_) }.x)
  println(grad(f)(3))