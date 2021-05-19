import scala.quoted.{Expr, Quotes}
import Poly._
import scala.compiletime._

object Macros:
  private def check(e: Expr[Poly])(using Quotes): Expr[Poly] =
      e match 
        case '{ ($l: Poly) + ($r: Poly) } =>
          Expr.block(check(l) :: check(r) :: Nil,  e)
        case '{ ($a: V) * (X^$n) } if n.valueOrError > 1 => e
        case '{ ($a: V) * X } => e
        case '{ X ^ $n } if n.valueOrError > 1 => e
        case '{ X } => e
        case '{ $a: V } => e
        case _ => '{ error("Not a simple polynomial") }

  inline def checkSimplePolynomial(inline e: Poly): Poly =
    ${check('e)}
