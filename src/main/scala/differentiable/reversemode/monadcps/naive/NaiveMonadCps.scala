package differentiable.reversemode.monadcps.naive;

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.language.implicitConversions
import scala.quoted._

trait DualMonad:
  def flatMap(k: Dual => DualMonad): DualMonad
  def map(k: Dual => Dual): DualMonad
//  def get: Dual

case class Dual(x: Double, var d: Double):
  thisDual =>

  def +(r: Dual): DualMonad = new DualMonad {
    override def flatMap(k: Dual => DualMonad): DualMonad =
      val y = Dual(x + r.x, 0)
      val res = k(y)
      d += y.d
      r.d += y.d
      res

    override def map(k: Dual => Dual): DualMonad =
      def wrap(dual: Dual): DualMonad = dual + 0
      flatMap(dual => wrap(k(dual)))
  }

  def *(thatDual: Dual): DualMonad = new DualMonad {
    override def flatMap(k: Dual => DualMonad): DualMonad =
      val parent = Dual(thisDual.x * thatDual.x, 0)
      val result = k(parent)
      thisDual.d += thatDual.x * parent.d
      thatDual.d += thisDual.x * parent.d
      result

    override def map(k: Dual => Dual): DualMonad =
      def wrap(dual: Dual): DualMonad = dual * 1
      flatMap(dual => wrap(k(dual)))
  }
end Dual


given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def grad(f: Dual => (Dual => DualMonad) => DualMonad)(x: Double): Double = {
  val xDual = Dual(x, 0)
  f(xDual) { top =>
    top.d = 1
    top * 1
  }
  xDual.d
}

def !(monad: DualMonad): Dual = ???
// !(2 * x) * x

@main def main() =
  def f(x: Dual)(k: Dual => DualMonad): DualMonad =
    for {
      y1 <- x * 2
      y2 <- x * x
      y3 <- y2 * x
      y4 <- y1 + y3
      y5 <- k(y4)
    } yield y5
  end f

  println(grad(f)(3))
end main
