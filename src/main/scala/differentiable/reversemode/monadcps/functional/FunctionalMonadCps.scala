package differentiable.reversemode.monadcps.functional

import differentiable.reversemode.monadcps.functional.DualMonad.wrap

import java.lang.Math.{pow, toIntExact}
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

case class DualMonad(parent: Num, adjointsUpdater: Adjoints => Adjoints):
  def flatMap(k: Num => DualMonad): DualMonad =
    val that = k(parent)
    DualMonad(that.parent, that.adjointsUpdater andThen this.adjointsUpdater )

  def map(k: Num => Num): DualMonad =
    flatMap(k andThen wrap)

object DualMonad:
  def wrap(n: Num): DualMonad = DualMonad(n, identity)

class Num(val x: Double):
  def *(that: Num): DualMonad =
    val parent = Num(this.x * that.x)

    def addPartialAdjoint(thisOrThat: Num, derivativeWrtThisOrThat: Double, adjoints: Adjoints) =
      val partialAdjoint = adjoints(parent) * derivativeWrtThisOrThat
      val addedPartialAdjoint = adjoints(thisOrThat) + partialAdjoint
      adjoints + (thisOrThat -> addedPartialAdjoint)

    DualMonad(
      parent,
      adjoints =>
        val adjointsWithThis = addPartialAdjoint(this, that.x, adjoints)
        val adjointsWithThat = addPartialAdjoint(that, this.x, adjointsWithThis)
        adjointsWithThat
    )
  end *

  def +(that: Num): DualMonad =
    val y = Num(this.x + that.x)

    def addPartialAdjoint(deriv: Adjoints, key: Num, y: Num)(op: (yd: Double) => Double): Adjoints =
      deriv + (key -> (
        deriv(key) + op(deriv(y))
        ))

    DualMonad(
      y,
      deriv =>
        val derivWithThis = addPartialAdjoint(deriv, this, y){ identity }
        addPartialAdjoint(derivWithThis, that, y){ identity }
    )
  end +

  override def toString: String = x.toString
end Num

given Conversion[Double, Num] = Num(_)
given Conversion[Int, Num] = Num(_)

def grad(f: DualMonad => DualMonad)(x: Double): Double =
  val xM = wrap(Num(x))
  val topMonad = f(xM)
  val initialDeriv = Map.empty.withDefaultValue(0.0).updated(topMonad.parent, 1.0)
//  println(topMonad)
  topMonad.adjointsUpdater(initialDeriv)(xM.parent)

type Adjoints = Map[Num, Double]

@main def main() =
  def f(xM: DualMonad): DualMonad =
    for
      x <- xM
      y1 <- x * 2
      y2 <- x * x
      y3 <- y2 * x // y3' = y2' * x + y2 * x'
      y4 <- y1 + y3
    yield y4

  def g(xM: DualMonad): DualMonad =
    for
      x <- xM
      y <- x * x
    yield y

  println(grad(f andThen g)(3))