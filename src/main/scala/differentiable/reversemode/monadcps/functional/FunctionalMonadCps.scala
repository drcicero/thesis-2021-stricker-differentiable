package differentiable.reversemode.monadcps.functional

import differentiable.reversemode.monadcps.functional.M.wrap

import java.lang.Math.pow
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

case class M(val y: Num, val d: Deriv, derivationUpdater: (Num, Deriv) => Deriv):
  def flatMap(k: Num => M): M =
    val m = k(y)
    M(y, derivationUpdater(y, m.d), (_, d) => d)

  def map(k: Num => Num): M =
    flatMap(n => wrap(k(n), d.updated(y, 1))) // TODO: remove finalization to make f chainable
end M

object M:
  def wrap(n: Num, d: Deriv = defaultMap): M =
    M(n, d, (_, d) => d)

case class Num(x: Double):
  def +(r: Num): M = M(
    x + r.x,
    defaultMap,
    (y, d) =>
      d
        .updated(this, d(this) + d(y))
        .updated(r, d(r) + d(y))
  )

  def *(r: Num): M = M(
    x * r.x,
    defaultMap,
    (y, d) =>
      d
        .updated(this, d(this) + r.x * d(y))
        .updated(r, d(r) + x * d(y))
  )
end Num

given Conversion[Double, Num] = Num(_)
given Conversion[Int, Num] = Num(_)

def grad(f: M => M)(x: Double): Double =
  val xM = wrap(Num(x))
  val d = f(xM).d
  d(xM.y)

val defaultMap : Deriv = Map.empty.withDefaultValue(0)
type Deriv = Map[Num, Double]

@main def main() =
  def f(xM: M): M =
    for {
      x <- xM
      y1 <- x * 2
      y2 <- x * x
      y3 <- y2 * x // y3' = y2' * x + y2 * x'
      y4 <- y1 + y3
    } yield y4
  end f

  println(grad(f)(3))