package differentiable.dualnumber

import differentiable.dualnumber._
import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class DualNumberTest extends AnyFunSuite {
  implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)

  test("dual number differentiation") {
    val cases: Seq[(Poly, Poly)] = Seq(
      X * X ->
        2 * X,
      5.5 * (X**3) ->
        16.5 * (X**2),
      (X**3) * (X**2) * (X**1) ->
        6 * (X ** 5),
      (X**3) * (X ** -2) * (X ** 1) ->
        2 * (X ** 1),
      0 + 0 * -0.1 + (X ** -5) * 0 ->
        0,
      X * 5 ->
        5,
      0 + 0 * -0.1 + (X**8) * (X**0) * (X**2) + (X**4) * X * (X**2) * (X**1) * (X ** -1) * (X**3) ->
        (10 * (X**9) + 10 * (X**9)),
      (5 * (X**2) + 2) ** 3 ->
        30 * X * ((5 * (X**2) + 2) ** 2)
    )

    cases.foreach { (actual, expectedDerivative) =>
      (-5 to 5).foreach { x =>
        val actualDerivativeOption =
          try Some(actual.d(x)) catch case _: IllegalArgumentException => None
        actualDerivativeOption.foreach { actualDerivative =>
          assert(
            actualDerivative === expectedDerivative(x),
            s"\nx = $x\nActual: $actual\nExpected: $expectedDerivative")
        }

      }
    }
  }
}
