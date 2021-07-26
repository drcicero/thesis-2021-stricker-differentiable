package differentiable.reversemode.monadcps.functional

import differentiable.reversemode.monadcps.functional.Monad.wrap

import java.lang.Math.{pow, toIntExact}
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

case class Monad(val y: Num, derivationUpdater: Deriv => Deriv):
  def flatMap(k: Num => Monad): Monad =
    val that = k(y)
    Monad(that.y, that.derivationUpdater andThen this.derivationUpdater )

  def map(k: Num => Num): Monad =
    flatMap(k andThen wrap)

object Monad:
  def wrap(n: Num): Monad = Monad(n, identity)

class Num(val x: Double):
  def +(that: Num): Monad =
    val y = Num(this.x + that.x)
    Monad(
      y,
      deriv =>
        val derivWithThis = updateDeriv(deriv, this, y){ identity }
        updateDeriv(derivWithThis, that, y){ identity }
    )

  def *(that: Num): Monad =
    val y = Num(this.x * that.x)
    Monad(
      y,
      deriv =>
        val derivWithThis = updateDeriv(deriv, this, y){ that.x * _ }
        updateDeriv(derivWithThis, that, y){ this.x * _ }
    )

  private def updateDeriv(deriv: Deriv, key: Num, y: Num)(op: (yd: Double) => Double): Deriv =
    deriv + (key -> (
      deriv(key) + op(deriv(y))
      ))

  override def toString: String = x.toString
end Num

given Conversion[Double, Num] = Num(_)
given Conversion[Int, Num] = Num(_)

def grad(f: Monad => Monad)(x: Double): Double =
  val xM = wrap(Num(x))
  val topMonad = f(xM)
  val initialDeriv = Map.empty.withDefaultValue(0.0).updated(topMonad.y, 1.0)
  println(topMonad)
  topMonad.derivationUpdater(initialDeriv)(xM.y)

type Deriv = Map[Num, Double]

@main def main() =
  def f(xM: Monad): Monad =
    for
      x <- xM
      y1 <- x * 2
      y2 <- x * x
      y3 <- y2 * x // y3' = y2' * x + y2 * x'
      y4 <- y1 + y3
    yield y4

  def g(xM: Monad): Monad =
    for
      x <- xM
      y <- x * x
    yield y

  println(grad(f andThen g)(3))