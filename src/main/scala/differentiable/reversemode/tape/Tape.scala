package differentiable.reversemode.tape

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.quoted._
import scala.language.implicitConversions

var tape: Unit => Unit = _ => ()

case class Dual(x: Double, var adjoint: Double):
  def +(that: Dual): Dual =
    val localResult = Dual(x + that.x, 0)
    tape = ((_: Unit) => this.adjoint += localResult.adjoint) andThen tape
    tape = ((_: Unit) => that.adjoint += localResult.adjoint) andThen tape
    localResult

  def *(that: Dual): Dual =
    val localResult = Dual(this.x * that.x, 0)

    def addPartialAdjoint(
      thisOrThat: Dual,
      derivativeWrtThisOrThat: Double
    ): Unit => Unit =
      _ =>
        val partialAdjoint = localResult.adjoint * derivativeWrtThisOrThat
        thisOrThat.adjoint += partialAdjoint

    tape = addPartialAdjoint(this, that.x) andThen tape
    tape = addPartialAdjoint(that, this.x) andThen tape

    localResult

  def **(n: Int): Dual =
    require(n >= 0)
    if n == 0 then 1 else this * (this ** (n - 1))

given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def grad(f: Dual => Dual)(x: Double): Double =
  tape = _ => ()
  val xDual: Dual = Dual(x, 0)
  f(xDual).adjoint = 1
  tape(())
  xDual.adjoint

def grad(xs: List[Double])(f: List[Dual] => Dual): List[Double] =
  tape = (_: Unit) => ()
  val xsDual: List[Dual] = xs map { Dual(_, 0) }
  f(xsDual).adjoint = 1
  tape(())
  xsDual map { _.adjoint }

@main def main() =
  def f(x: Dual): Dual =
    2 * x + x * x * x

  println(grad(f)(3))