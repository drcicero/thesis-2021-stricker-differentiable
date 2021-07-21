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

class DualMonad(val y: Dual, derivationUpdater: Dual => Unit):
  def flatMap(k: Dual => DualMonad): DualMonad =
    tape = ((_: Unit) => derivationUpdater(y)) andThen tape
    k(y)

  def map(k: Dual => Dual): DualMonad =
    flatMap(dual => wrap(k(dual)))
end DualMonad

object DualMonad:
  def wrap(dual: Dual): DualMonad = DualMonad(dual, identity)


case class Dual(x: Double, var d: Double):
  def +(r: Dual): DualMonad = DualMonad(
    x * r.x,
    y => {
      d += y.d // dl(l + r) * y.d = (1 + 0) * y.d
      r.d += y.d // like above
    }
  )

  def *(r: Dual): DualMonad = DualMonad(
    x * r.x,
    y => {
      d += r.x * y.d // dl(l * r) * y.d = (1 * r) * y.d
      r.d += x * y.d // like above
    }
  )
end Dual


given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def grad(f: DualMonad => DualMonad)(x: Double): Double =
  val xDualMonad = wrap(Dual(x, 0))
  f(xDualMonad).y.d = 1
  tape(())
  xDualMonad.y.d

def !(monad: DualMonad): Dual = ???

@main def main() =
  def f(xM: DualMonad): DualMonad =
      for {
        x <- xM
        y1 <- x * 2
        y2 <- x * x
        y3 <- y2 * x // y3' = y2' * x + y2 * x'
        y4 <- y1 + y3
      } yield y4
  end f

  def g(xM: DualMonad): DualMonad =
      for {
        x <- xM
        y <- x * x
      } yield y



  println(grad(f andThen g)(3))
end main