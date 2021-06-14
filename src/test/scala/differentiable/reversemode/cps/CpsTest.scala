package differentiable.reversemode.cps

import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class CpsTest extends AnyFunSuite {
  implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)

  test("reverse mode cps differentiation") {
    val cases: Seq[(Dual => (Dual => Dual) => Dual, Double => Double)] = Seq(
      ((x: Dual) => (k: Dual => Dual) =>
        (x * x) {
          k(_)
        }) ->
        (x => 2 * x),
      ((x: Dual) => (k: Dual => Dual) =>
        (2 * x) { y1 =>
          (x * x) { y2 =>
            (y2 * x) { y3 =>
              (y1 + y3) {
                k(_)
              }
            }
          }
        }) ->
        (x => 2 + 3 * x * x)
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
