package differentiable.reversemode.chad

import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

import java.lang.Math.pow

class ChadTest extends AnyFunSuite {
  test("reverse mode chad differentiation") {
    implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)

    import scala.language.implicitConversions
    /*
    * 2x^3 or 8x^3 + x^2
    * */
    def f(x: Dual[Real, Real]) =
      val a = mulTwo(x, 2)
      if x.v < 0 then
        mulTwo(mulTwo(x, x), a)
      else
        val b = mulTwo(mulTwo(x, x), 2)
        val c = sumTwo(mulTwo(4, b), x)
        mulTwo(c, x)

    /*
    * x^2 + 8x
    *
    * =f(<0)==> 4x^6 + 16x^3 =d=> 24x^5 + 48x^2
    * =f(>=0)=> (8x^3 + x^2)^2 + 64x^3 + 8x^2 =d=> 2*(24*x^2+2*x)*(8*x^3+x^2)+192*x^2+16*x
    *
    * */
    def g(x: Dual[Real, Real]) =
      val b = sumTwo(x, 8)
      mulTwo(b, x)

    def expectedFG(x: Double) =
      if x < 0 then
        24 * pow(x, 5) + 48 * pow(x, 2)
      else
        2 * (24 * pow(x, 2) + 2 * x) * (8 * pow(x, 3) + pow(x, 2)) + 192 * pow(x, 2) + 16 * x

    val cases: Seq[(Dual[Real, Real] => Dual[Real, Real], Double => Double)] = Seq(
      (f andThen g) -> expectedFG
    )

    cases.foreach { (actual, expectedDerivative) =>
      (-5 to 5).foreach { x =>
        assert(
          actual(variable(x)).d(1) === expectedDerivative(x),
          s"\nx = $x\nActual: $actual\nExpected: $expectedDerivative")


      }
    }
  }
}
