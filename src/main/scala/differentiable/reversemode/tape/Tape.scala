package differentiable.reversemode.tape

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.quoted._
import scala.language.implicitConversions

var tape: Unit => Unit = _ => ()

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

  def **(n: Int): Dual =
    require(n >= 0)
    if n == 0 then 1 else this * (this ** (n - 1))

given Conversion[Double, Dual] = Dual(_, 0)
given Conversion[Int, Dual] = Dual(_, 0)

def grad(x: Double)(f: Dual => Dual): Double =
  grad(x :: Nil) { case xDual :: Nil =>
    f(xDual)
  }
  .head

def grad(xs: List[Double])(f: List[Dual] => Dual): List[Double] =
  tape = (_: Unit) => ()
  val xsDual: List[Dual] = xs map identity
  f(xsDual).d = 1
  tape(())
  xsDual map { _.d }

@main def main() =
  def f(x: Dual): Dual =
    2 * x + x * x * x


  println(f(2))
  println(grad(2)(f))