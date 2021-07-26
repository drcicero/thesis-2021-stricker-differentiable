package differentiable.reversemode.tape.functional

import java.lang.Math.{pow, toIntExact}
import java.util.UUID
import java.util.UUID.randomUUID
import scala.annotation.showAsInfix
import scala.compiletime.ops.int._
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted._

case class Num(x: Double, derivUpdater: Deriv => Deriv, id: UUID):
  def +(that: Num): Num = op(that, _ + _, identity, identity)

  def *(that: Num): Num = op(that, _ * _, that.x * _, this.x * _)

  private def op(
                  that: Num,
                  numericOp: (Double, Double) => Double,
                  thisDerivSummand: (dy: Double) => Double,
                  thatDerivSummand: (dy: Double) => Double): Num =
    val yId = randomUUID()

    def updateDeriv(deriv: Deriv, key: UUID)(derivSummand: (dy: Double) => Double): Deriv =
      deriv + (key -> (
        /*
        We need the current derivation of y.
        Because y doesn't exist yet we use an ID which we generate beforehand to access dy
         */
        deriv(key) + derivSummand(deriv(yId))
      ))

    val additionalDerivUpdater = { (deriv: Deriv) =>
        val derivWithThis = updateDeriv(deriv, this.id){ thisDerivSummand }
        updateDeriv(derivWithThis, that.id){ thatDerivSummand }
      }
    val composedDerivUpdater =
      additionalDerivUpdater andThen this.derivUpdater andThen that.derivUpdater
    Num(
      numericOp(this.x, that.x),
      composedDerivUpdater,
      yId
    )
  end op

  override def toString: String = x.toString
end Num

object Num:
  def apply(x: Double, derivationUpdater: Deriv => Deriv): Num =
    Num(x, derivationUpdater, randomUUID())
  def apply(x: Double): Num = Num(x, identity)
end Num

type Deriv = Map[UUID, Double]

given Conversion[Double, Num] = Num(_)
given Conversion[Int, Num] = Num(_)


def grad(f: Num => Num)(x: Double): Double =
  val xNum = Num(x)
  val top = f(xNum)
  val initialDeriv = Map.empty.withDefaultValue(0.0).updated(top.id, 1.0)
  println(top)
  top.derivUpdater(initialDeriv)(xNum.id)


@main def main() =
  def f(x: Num): Num =
    2 * x + x * x * x

//  2 + 3 * x ^ 2 = 2 + 3 * 9 = 29
  println(grad(f)(3))