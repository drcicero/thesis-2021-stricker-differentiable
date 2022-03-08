package differentiable.reversemode.monadcps

import differentiable.reversemode.monadcps.DualMonad.wrap

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import scala.quoted._
import concurrent.ExecutionContext.Implicits.global

var tape: Unit => Unit = _ => {}

class DualMonad(val parent: Dual, val adjointsUpdater: Dual => Unit):
  def flatMap(k: Dual => DualMonad): DualMonad =
    tape = ((_: Unit) => adjointsUpdater(parent)) andThen tape
    k(parent)

  def map(k: Dual => Dual): DualMonad =
    flatMap(k andThen wrap)
end DualMonad

object DualMonad:
  def wrap(dual: Dual): DualMonad = DualMonad(dual, identity)


case class Dual(x: Double, var adjoint: Double):
  def *(that: Dual): DualMonad =
    def addPartialAdjoint(
                           thisOrThat: Dual,
                           derivativeWrtThisOrThat: Double,
                           parentAdjoint: Double
                         ): Unit =
      val partialAdjoint = parentAdjoint * derivativeWrtThisOrThat
      thisOrThat.adjoint += partialAdjoint

    DualMonad(
      this.x * that.x,
      parent =>
        addPartialAdjoint(this, that.x, parent.adjoint)
        addPartialAdjoint(that, this.x, parent.adjoint)
    )

  def +(r: Dual): DualMonad = DualMonad(
    x * r.x,
    y => {
      adjoint += y.adjoint // dl(l + r) * y.d = (1 + 0) * y.d
      r.adjoint += y.adjoint // like aboved
    }
  )
end Dual


given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def grad(f: DualMonad => DualMonad)(x: Double): Double =
  tape = _ => {}
  val xDualMonad = wrap(Dual(x, 0))
  f(xDualMonad).parent.adjoint = 1
  tape(())
  xDualMonad.parent.adjoint

def !(monad: DualMonad): Dual = ???

@main def main() =
  def f(xM: DualMonad): DualMonad =
    for
      x <- xM
      y1 <- x * 2
      y2 <- x * x
      y3 <- y2 * x // y3' = y2' * x + y2 * x'
      y4 <- y1 + y3
    yield y4
  end f

  def g(xM: DualMonad): DualMonad =
    for {
      x <- xM
      y <- x * x
    } yield y

  println(grad(f andThen g)(3))
end main