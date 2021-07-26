package differentiable.reversemode.monadcps.functional.forless

import scala.quoted._

inline def !(inline n: Num): Monad = ${impl('n)}

private def impl(n: Expr[Num])(using Quotes): Expr[Monad] = ???