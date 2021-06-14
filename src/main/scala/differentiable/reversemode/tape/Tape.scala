package differentiable.reversemode.tape

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.quoted._
import scala.language.implicitConversions
import scala.async.Async.{async, await}

var tape: Unit => Unit = (_: Unit) => ()

case class Dual(x: Double, var d: Double):
  def +(r: Dual): Dual =
    val y = Dual(x + r.x, 0)
    tape = ((_: Unit) => d += y.d) andThen tape
    tape = ((_: Unit) => r.d += y.d) andThen tape
    y

  def *(r: Dual): Dual =
    val y = Dual(x * r.x, 0)
    tape = ((_: Unit) => d += r.x * y.d) andThen tape
    tape = ((_: Unit) => r.d += x * y.d) andThen tape
    y

given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def grad(f: Dual => Dual)(x: Double): Double =
  val xDual = Dual(x, 0)
  f(xDual).d = 1
  tape(())
  xDual.d

@main def main() =
  def f(x: Dual): Dual =
    2 * x + x * x * x


  println(f(2))
  println(grad(f)(2))