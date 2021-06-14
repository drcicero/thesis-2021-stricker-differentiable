package differentiable.reversemode.tape

import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class TapeTest extends AnyFunSuite {
  implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)

  test("reverse mode tape differentiation") {
    val cases: Seq[(Dual => Dual, Double => Double)] = Seq(
      ((x: Dual) => x * x) ->
        (x => 2 * x),
      ((x: Dual) => 5.5 * (x * x * x)) ->
        (x => 16.5 * (x * x)),
      ((x: Dual) => 2 * x + x * x * x) ->
        (x => 2 + 3 * x * x),
      ((x: Dual) => x * 5) ->
        (x => 5)
    )

    cases.foreach { (actual, expectedDerivative) =>
      (-5 to 5).foreach { x =>
        assert(
          grad(actual)(x) === expectedDerivative(x),
          s"\nx = $x\nActual: $actual\nExpected: $expectedDerivative")


      }
    }
  }
}
