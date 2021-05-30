package differentiable.matchtype

import differentiable.matchtype._
import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

class MatchTypeTest extends AnyFunSuite {
  implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)

  test("match type differentiation") {
    val cases = Seq(
      d[X * X] -> initPolynomial[
        V[2] * X
      ],
      d[V[5.5] * (X**3)] -> initPolynomial[
        V[16.5] * (X**2)
      ],
      d[(X**3) * (X**2) * (X**1)] -> initPolynomial[
        V[6] * (X ** 5)
      ],
      d[(X**3) * (X ** -2) * (X**1)] -> initPolynomial[
        V[2] * (X**1)
      ],
      d[V[0] + V[0] * V[-0.1] + (X ** -5) * V[0]] -> initPolynomial[
        V[0]
      ],
      d[X * V[5]] -> initPolynomial[
        V[5]
      ],
      // compile time is much longer with 4 multiplications
      d[V[0] + V[0] * V[-0.1] + (X ** 10) * (X ** 0) * (X ** 2) + (X**3) * X * (X**2)] -> initPolynomial[
        V[12] * (X**11) + V[6] * (X**5)
      ]
    )
    cases.foreach { (actual, expected) =>
      (-5 to 5).foreach { x =>
        assert(actual(x) === expected(x), s"\nx = $x\nActual: $actual\nExpected: $expected")
      }
    }
  }
}
