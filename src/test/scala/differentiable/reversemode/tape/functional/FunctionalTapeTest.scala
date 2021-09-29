package differentiable.reversemode.tape.functional

import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.funsuite.AnyFunSuite

import java.lang.Math.{pow, random}
import scala.annotation.tailrec
import scala.language.implicitConversions

class FunctionalTapeTest extends AnyFunSuite {

  test("reverse mode tape differentiation") {
    implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.0001)

    /*
    * 2x^3 or 8x^3 + x^2
    * */
    def f(x: Num) =
      val a = x * 2
      if x.x < 0 then
        (x ** 2) * a
      else
        val b = 2 * (x ** 2)
        val c = 4 * b + x
        c * x

    /*
    * x^2 + 8x
    *
    * =f(<0)==> 4x^6 + 16x^3 =d=> 24x^5 + 48x^2
    * =f(>=0)=> (8x^3 + x^2)^2 + 64x^3 + 8x^2 =d=> 2*(24*x^2+2*x)*(8*x^3+x^2)+192*x^2+16*x
    *
    * */
    def g(x: Num) =
      val b = x + 8
      b * x

    def expectedFG(x: Double) =
      if x < 0 then
        24 * pow(x, 5) + 48 * pow(x, 2)
      else
        2 * (24 * pow(x, 2) + 2 * x) * (8 * pow(x, 3) + pow(x, 2)) + 192 * pow(x, 2) + 16 * x

    val cases: Seq[(Num => Num, Double => Double)] = Seq(
      ((x: Num) => x * x) ->
        (x => 2 * x),
      ((x: Num) => 5.5 * (x ** 3)) ->
        (x => 16.5 * (x * x)),
      ((x: Num) => 2 * x + x * x * x) ->
        (x => 2 + 3 * x * x),
      ((x: Num) => x * 5) ->
        (x => 5),
      ((x: Num) => 0 + 0 * -0.1 + (x ** 8) * (x ** 0) * (x ** 2) + (x ** 4) * x * (x ** 2) * (x ** 1) * (x ** 3)) ->
        (x => 10 * pow(x, 9) + 11 * pow(x, 10)),
      (f andThen g) -> expectedFG
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
