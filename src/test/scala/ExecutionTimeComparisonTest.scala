import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import java.lang.Math.{pow, random}
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.language.implicitConversions

class ExecutionTimeComparisonTest extends AnyFunSuite {
  // maybe use relative equality
  implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.000000000001)

  def expectedDerivative(x: Double) =
    0.3 * x * x + 8 * x + 31.6

  val xs = Range.BigDecimal.inclusive(-10, 10, 0.0001).map(_.toDouble)
  val expectedDiffs = xs map expectedDerivative

  def assertAndTime(grad: Double => Double, testName: String) =
    val startTimeNano = System.nanoTime()
    val actualDiffs = xs map grad
    val endTimeNano = System.nanoTime()
    val excecutionTimeMilli = TimeUnit.NANOSECONDS.toMillis(endTimeNano - startTimeNano)

    print(s"$testName: $excecutionTimeMilli ms")

    expectedDiffs zip actualDiffs zip xs foreach { case ((expected, actual), x) =>
      assert(expected === actual, s"x=$x")
    }


  test("forward dual number") {
    import differentiable.dualnumber.{_, given}

    def f(x: Dual): Dual =
    // 0.1x^3 + 2x^2 + 0.6x + 2x^2 + 31x + x - x = 0.1x^3 + 4x^2 + 31.6x =d=> 0.3x^2 + 8x + 31.6
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x

    assertAndTime(differentiate(f), "forward dual number")
  }

  test("forward dual number macro") {
    import differentiable.dualnumber.{_, given}

    assertAndTime(
      macroDifferentiate(x => 0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x),
      "forward dual number macro"
    )
  }

  test("forward match type") {
    import differentiable.matchtype.{_, given}

    type F = V[0.1] * X * X * X + V[2.0] * X * (X + V[0.3] + X * V[1.0]) + V[31.0] * X + X + (V[-1.0] * X)
    //    type F[Y] = V[0.1] * X[Y] * X[Y] * X[Y] + V[2.0] * X[Y] * (X[Y] + V[0.3] + X[Y] * V[1.0]) + V[31.0] * X[Y] + X[Y] + (V[-1.0] * X[Y])

    assertAndTime(d[F].apply, "forward match type")
  }


  test("forward polynomial macro") {
    import differentiable.macros.{_, given}

    assertAndTime(
      d(
        0.1 * X * X * X + 2 * X * (X + 0.3 + X * 1) + 31 * X + X + -1 * X
      ).apply,
      "forward polynomial macro")
  }

  test("reverse cps") {
    import differentiable.reversemode.cps.{_, given}

    def f(x: Dual)(k: Dual => Dual): Dual =
      (0.1 * x) { a1 =>
        (a1 * x) { a2 =>
          (a2 * x) { a3 =>
            (x * 1) { b1 =>
              (b1 + 0.3) { b2 =>
                (b2 + x) { b3 =>
                  (b3 * x) { c1 =>
                    (c1 * 2) { c2 =>
                      (31 * x) { d =>
                        (-1 * x) { e =>
                          (d + x) { f1 =>
                            (f1 + e) { f2 =>
                              (a3 + c2) { g1 =>
                                (g1 + f2) {
                                  k
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    end f

    assertAndTime(differentiate(f), "reverse cps")
  }

  test("reverse cps functional") {
    import differentiable.reversemode.cps.functional.{_, given}

    def f(x: Num)(k: Cont): Deriv =
      (0.1 * x) { a1 =>
        (a1 * x) { a2 =>
          (a2 * x) { a3 =>
            (x * 1) { b1 =>
              (b1 + 0.3) { b2 =>
                (b2 + x) { b3 =>
                  (b3 * x) { c1 =>
                    (c1 * 2) { c2 =>
                      (31 * x) { d =>
                        (-1 * x) { e =>
                          (d + x) { f1 =>
                            (f1 + e) { f2 =>
                              (a3 + c2) { g1 =>
                                (g1 + f2) {
                                  k
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    end f

    assertAndTime(grad(f), "reverse cps functional")
  }

  test("reverse tape") {
    import differentiable.reversemode.tape.{_, given}

    def f(x: Dual): Dual =
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x

    assertAndTime(grad(f), "reverse tape")
  }

  test("reverse tape functional") {
    import differentiable.reversemode.tape.functional.{_, given}

    def f(x: Num): Num =
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x

    assertAndTime(grad(f), "reverse tape functional")
  }

  // StackOverflow :(
  /*
  test("reverse monad") {
    def f(xM: monadcps.DualMonad): monadcps.DualMonad =
      for {
        x <- xM

        a1 <- 0.1 * x
        a2 <- a1 * x
        a3 <- a2 * x

        b1 <- x * 1
        b2 <- b1 + 0.3
        b3 <- b2 + x

        c1 <- b3 * x
        c2 <- c1 * 2

        d <- 31 * x
        e <- -1 * x

        f1 <- d + x
        f2 <- f1 + e

        g1 <- a3 + c2
        g2 <- g1 + f2
      } yield g2
    end f

    assertAndTime(monadcps.grad(f), "reverse monad")
  }
  */

  test("reverse monad functional") {
    import differentiable.reversemode.monadcps.functional.{_, given}

    def f(xM: Monad): Monad =
      for {
        x <- xM

        a1 <- 0.1 * x
        a2 <- a1 * x
        a3 <- a2 * x

        b1 <- x * 1
        b2 <- b1 + 0.3
        b3 <- b2 + x

        c1 <- b3 * x
        c2 <- c1 * 2

        d <- 31 * x
        e <- -1 * x

        f1 <- d + x
        f2 <- f1 + e

        g1 <- a3 + c2
        g2 <- g1 + f2
      } yield g2
    end f

    assertAndTime(grad(f), "reverse monad functional")
  }

  test("reverse monad functional forless") {
    import differentiable.reversemode.monadcps.functional.forless.{_, given}

    def f(xM: Monad): Monad =
      forless {
        val x = !xM

        val a1 = !(0.1 * x)
        val a2 = !(a1 * x)
        val a3 = !(a2 * x)

        val b1 = !(x * 1)
        val b2 = !(b1 + 0.3)
        val b3 = !(b2 + x)

        val c1 = !(b3 * x)
        val c2 = !(c1 * 2)

        val d = !(31 * x)
        val e = !(-1 * x)

        val f1 = !(d + x)
        val f2 = !(f1 + e)

        val g1 = !(a3 + c2)
        (g1 + f2)._yield
      }
    end f

    assertAndTime(grad(f), "reverse monad functional forless")
  }

  test("reverse chad") {
    import differentiable.reversemode.chad.{_, given}


    def f(x: Dual[Double, Double]): Dual[Double, Double] =
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x

    assertAndTime(x => f(variable(x)).d(1), "reverse chad")
  }


  /*
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
    val startTime = System.nanoTime()
    (Range.BigDecimal.inclusive(-10, 10, 0.1)).foreach { x =>
      assert(
        grad(x.toDouble)(actual) === expectedDerivative(x.toDouble),
        s"\nx = $x\nActual: $actual\nExpected: $expectedDerivative")
    }
    val endTime = System.nanoTime()
    println((endTime - startTime) * 1e-9)
  }
}*/
}
