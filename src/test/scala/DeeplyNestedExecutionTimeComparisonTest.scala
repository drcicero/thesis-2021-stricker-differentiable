import org.scalactic.Equality
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import java.lang.Math.{pow, random}
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.language.implicitConversions

class DeeplyNestedExecutionTimeComparisonTest extends AnyFunSuite {
  // maybe use relative equality
  implicit val doubleEq: Equality[Double] = tolerantDoubleEquality(0.000000000001)

  val nestingStep: Int = 10
  val nestingStart: Int = 0
  val globalNestingStop: Int = 3000

  val n = 100000
  val xs = Range.BigDecimal(0, n, 1).map(_.toDouble)

  def time(grad: Double => Double, testName: String = "test") =
    var i = 0
    val startTimeNano = System.nanoTime()
    while i < n do
      grad(i)
      i += 1
    val endTimeNano = System.nanoTime()


//    val excecutionTimeMilli = TimeUnit.NANOSECONDS.toMillis(endTimeNano - startTimeNano)
    val executionTimeNano = endTimeNano - startTimeNano
    val executionTimeOneExpression = executionTimeNano / n

//    print(s"$testName: $executionTimeOneExpression ns")
    executionTimeOneExpression

  ignore("forward dual number") {
    import differentiable.dualnumber.{_, given}

    def f(x: Dual): Dual =
    // 0.1x^3 + 2x^2 + 0.6x + 2x^2 + 31x + x - x = 0.1x^3 + 4x^2 + 31.6x =d=> 0.3x^2 + 8x + 31.6
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x

    val nestingStop = 2000
    timeNestings(differentiate, f, nestingStop, "forward dual number")

  }

  private def timeNestings[A](differentiate: (A => A) => Double => Double, f: A => A, nestingStop: Int, testName: String): Unit =
    def timeOneNesting(i: Int) =
      val result = time(differentiate((0 until i).foldLeft(f)((l, _) => l andThen f)))
      s"$i, $result"

    timeNestingsWithIncreasingStep(timeOneNesting, nestingStop, testName)

  private def timeNestingsWithIncreasingStep(timeOneNesting: Int => String, nestingStop: Int, testName: String): Unit =
    def fromToBy(from:Int, until: Int, by: Int) = from until (until min (nestingStop + 1) min globalNestingStop) by by map timeOneNesting

    val times =
      fromToBy(nestingStart, 10, 1) appendedAll
        fromToBy(10, 150, 10) appendedAll
        fromToBy(150, 300, 25) appendedAll
        fromToBy(300, 1000000, 500)



    print(s"$testName: ${times.mkString("(", ") (", ")")}")


  ignore("reverse cps") {
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

    val nestingStop = 130
    def timeOneNesting(i: Int) =
      val result = time(differentiate(
        (0 until i).foldLeft(f) { (l, _) =>
          x =>
            k =>
              l(x)(r => f(r)(k))

        }))
      s"$i, $result"
    end timeOneNesting

    timeNestingsWithIncreasingStep(timeOneNesting, nestingStop, "reverse cps")
  }

  test("reverse cps functional") {
    import differentiable.reversemode.cps.functional.{_, given}

    def f(x: Num)(k: Continuation): Adjoints =
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

    val nestingStop = 140

    def timeOneNesting(i: Int) =
      val result = time(grad(
        (0 until i).foldLeft(f) { (l, _) =>
          x =>
            k =>
              l(x)(r => f(r)(k))

        }))
      s"$i, $result"

    timeNestingsWithIncreasingStep(timeOneNesting, nestingStop, "reverse cps functional")
  }

  test("reverse tape") {
    import differentiable.reversemode.tape.{_, given}

    def f(x: Dual): Dual =
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x

    val nestingStop = 280//130
    timeNestings(grad, f, nestingStop, "reverse tape")
  }

/*
  test("reverse tape functional") {
    import differentiable.reversemode.tape.functional.{_, given}

    def f(x: Num): Num =
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x

    assertAndTime(grad(nestingsRange.foldLeft(f)((l, _) => l andThen f)), "reverse tape functional")
  }
*/


  test("reverse monad") {
    import differentiable.reversemode.monadcps.{_, given}

    def f(xM: DualMonad): DualMonad =
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

    val nestingStop = 280
    timeNestings(grad, f, nestingStop, "reverse monad")
  }


  ignore("reverse monad functional") {
    import differentiable.reversemode.monadcps.functional.{_, given}

    def f(xM: DualMonad): DualMonad =
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

    val nestingStop = 7000 // 7000 still works
    timeNestings(grad, f, nestingStop, "reverse monad functional")
  }

  ignore("reverse monad functional forless") {
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

    val nestingStop = 7000 // 7000 still works
    timeNestings(grad, f, nestingStop, "reverse monad functional forless")
  }

  test("reverse chad") {
    import differentiable.reversemode.chad.{_, given}


    def f(x: Dual[Double, Double]): Dual[Double, Double] =
      0.1 * x * x * x + 2 * x * (x + 0.3 + x * 1) + 31 * x + x + -1 * x


    def differentiate(f: Dual[Double, Double] => Dual[Double, Double])(x: Double) =
      f(variable(x)).d(1)

    timeNestings(differentiate, f, 5, "reverse chad")
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
