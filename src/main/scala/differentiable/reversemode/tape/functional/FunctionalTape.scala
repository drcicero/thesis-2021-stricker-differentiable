package differentiable.reversemode.tape.functional

import java.lang.Math.{pow, toIntExact}
import java.util.UUID
import java.util.UUID.randomUUID
import scala.annotation.showAsInfix
import scala.collection.immutable.ListSet
import scala.compiletime.ops.int.*
import scala.compiletime.{constValue, erasedValue}
import scala.language.implicitConversions
import scala.quoted.*

case class Num(x: Double, derivUpdater: ListSet[Deriv => Deriv], id: UUID):
  def +(that: Num): Num = op(that, _ + _, identity, identity)

  def *(that: Num): Num = op(that, _ * _, that.x * _, this.x * _)

  def **(n: Int): Num = n match
    case 0 => 1
    case _ if n < 0 => throw IllegalArgumentException("Exponent < 0")
    case _ => List.fill(n) { this } reduce {_ * _}

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
    // If we already visited a node (because it was saved in a val and is used multiple times) then we don't want to
    // add its derivUpdater multiple times. Therefore a ListSet is used to save the "tape"
    val composedDerivUpdater =
      ListSet(additionalDerivUpdater) ++ this.derivUpdater ++ that.derivUpdater
    Num(
      numericOp(this.x, that.x),
      composedDerivUpdater,
      yId
    )
  end op

  override def toString: String = x.toString
end Num

object Num:
  def apply(x: Double, derivUpdater: ListSet[Deriv => Deriv]): Num =
    Num(x, derivUpdater, randomUUID())
  def apply(x: Double): Num = Num(x, ListSet(identity))
end Num

type Deriv = Map[UUID, Double]

given Conversion[Double, Num] = Num(_)
given Conversion[Int, Num] = Num(_)


def grad(f: Num => Num)(x: Double): Double =
  val xNum = Num(x)
  val top = f(xNum)
  val initialDeriv = Map.empty.withDefaultValue(0.0).updated(top.id, 1.0)
//  println(top)
  top.derivUpdater.reduceLeft(_ andThen _)(initialDeriv)(xNum.id)


@main def main() =
  def f(x: Num): Num =
    val b = 2 * x
    if b.x > 5 then b else 2
//    (b + b) * b

  def g(x: Num): Num =
    val b = x * x
    b + b


  //2(4x * 2x)^2 = 2(8x^2)^2 = 2*64x^4 = 128x^4 =>
  println(grad(f andThen g)(3))