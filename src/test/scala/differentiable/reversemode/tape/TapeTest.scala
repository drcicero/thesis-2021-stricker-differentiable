package differentiable.reversemode.tape

import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

import java.lang.Math.{pow, random}
import scala.annotation.tailrec
import scala.language.implicitConversions

class TapeTest extends AnyFunSuite {

  test("reverse mode tape differentiation") {
    implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)
    val cases: Seq[(Dual => Dual, Double => Double)] = Seq(
      ((x: Dual) => x * x) ->
        (x => 2 * x),
      ((x: Dual) => 5.5 * (x ** 3)) ->
        (x => 16.5 * (x * x)),
      ((x: Dual) => 2 * x + x * x * x) ->
        (x => 2 + 3 * x * x),
      ((x: Dual) => x * 5) ->
        (x => 5),
      ((x: Dual) => 0 + 0 * -0.1 + (x ** 8) * (x ** 0) * (x ** 2) + (x ** 4) * x * (x ** 2) * (x ** 1) * (x ** 3)) ->
        (x => 10 * pow(x, 9) + 11 * pow(x, 10))
    )

    cases.foreach { (actual, expectedDerivative) =>
      (-5 to 5).foreach { x =>
        assert(
          grad(x)(actual) === expectedDerivative(x),
          s"\nx = $x\nActual: $actual\nExpected: $expectedDerivative")


      }
    }
  }

  test("reverse mode tape learning") {
    val realWs: List[Double] = List(10, -7, -5, 4)
    def trainPoly(x: Double)(ws: List[Double]): Double =
      poly(x)(ws map identity).x

    def poly(x: Double)(ws: List[Dual]): Dual =
      ws
        .zipWithIndex
        .map { (w, i) =>
          w * (x ** i)
        }
        .reduce(_ + _)

    val trainInput = Range.BigDecimal(-5, 5, 1)

    def loss(x: Double)(ws: List[Dual]): Dual =
      (trainPoly(x)(realWs) + -1 * poly(x)(ws)) ** 2

    @tailrec def learn(ws: List[Double], rate: Double, stopAcc: Double, maxIterations: Int): List[Double] =
      val lossAverage =
        trainInput
          .map { x =>
            val wsDual: List[Dual] = ws map identity
            loss(x.toDouble)(wsDual).x / ws.length / trainInput.length
          }
          .sum
//      println(s"It: $maxIterations Loss: $lossAverage, Current Ws: $ws")
      if lossAverage < stopAcc || maxIterations <= 0 || !lossAverage.isFinite then
        ws
      else
        val dLossAverage: List[Double] =
          trainInput
            .map { x =>
              grad(ws)(loss(x.toDouble)) map { _ / trainInput.length }
            }
            .reduce {
              _ zip _ map {
                _ + _
              }
            }
        val newWs =
          ws zip dLossAverage map { (w, dLoss) =>
            w - rate / ws.length * dLoss
          }
        learn(newWs, rate, stopAcc, maxIterations - 1)
      end if
    end learn

    val initialWs = (0 to 3 map { _ => 300 * random - 150 }).toList
    val acc = 1E-9
    val learnedWs = learn(initialWs, 0.0007, acc, 100000)

    implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)
    assert(realWs zip learnedWs map { _ === _ } reduce { _ && _ }, s"\nreal: $realWs\nlearned: $learnedWs")
    println(s"Init: $initialWs")
    println(s"Real: $realWs")
    println(s"Learned: $learnedWs")
  }
}
