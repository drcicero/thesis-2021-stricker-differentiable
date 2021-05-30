package differentiable.macros

import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class MacroTest extends AnyFunSuite {
  implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)
  
  test("macro differentiation") {
    val cases: Seq[(Poly, Poly)] = Seq(
      d(X * X) ->
        2 * X,
      d(5.5 * (X**3)) ->
        16.5 * (X**2),
      d((X**3) * (X**2) * (X**1)) ->
        6 * (X ** 5),
      d((X**3) * (X ** -2) * (X ** 1)) ->
        2 * (X ** 1),
      d(0 + 0 * -0.1 + (X ** -5) * 0) ->
        0,
      d(X * 5) ->
        5,
      d(0 + 0 * -0.1 + (X**8) * (X**0) * (X**2) + (X**4) * X * (X**2) * (X**1) * (X ** -1) * (X**3)) ->
        (10 * (X**9) + 10 * (X**9)),
      d((5 * (X**2) + 2) ** 3) ->
        30 * X * ((5 * (X**2) + 2) ** 2)
    )

    cases.foreach { (actual, expected) =>
      println(actual)
      (-5 to 5).foreach { x =>
        val actualValueOption =
          try Some(actual(x)) catch case _: IllegalArgumentException => None
        actualValueOption.foreach { actualValue =>
          assert(
            actualValue === expected(x),
            s"\nx = $x\nActual: $actual\nExpected: $expected")
        }
      }
    }
  }
}
