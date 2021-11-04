package differentiable.reversemode.cps

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

case class Dual(x: Double, var adjoint: Double):
  def *(that: Dual)(k: Dual => Dual): Dual =
    val localResult = Dual(this.x * that.x, 0)
    val globalResult = k(localResult)
    
    this.adjoint += that.x * localResult.adjoint
    that.adjoint += this.x * localResult.adjoint  

    globalResult
  
  def +(that: Dual)(k: Dual => Dual): Dual =
    val localResult = Dual(this.x + that.x, 0)
    val globalResult = k(localResult)
    
    this.adjoint += 1 * localResult.adjoint
    that.adjoint += 1 * localResult.adjoint 
    
    globalResult
    
given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def differentiate(f: Dual => (Dual => Dual) => Dual)(x: Double): Double = {
  val xDual = Dual(x, 0)
  f(xDual) { topExpression => {
    topExpression.adjoint = 1
    topExpression
  }
  }
  xDual.adjoint
}


@main def main() =
  def f(x: Dual)(k: Dual => Dual): Dual =
    (2 * x) { y1 =>
      (x * x) { y2 =>
        (y2 * x) { y3 =>
          (y1 + y3) { k }
        }
      }
    }

  println(differentiate(f)(3))

