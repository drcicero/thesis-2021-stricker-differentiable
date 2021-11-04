package differentiable.dualnumber

import scala.quoted._

private def diff(f: Expr[Dual => Dual])(using Quotes): Expr[Double => Double] = 
  '{ x => ${ d( Expr.betaReduce('{ $f(Dual(x, 1)) }) ) }  }

private def d(dual: Expr[Dual])(using Quotes): Expr[Double] = dual match
  case '{ ($l: Dual) + ($r: Dual) } =>
    '{ ${ d(l) } + ${ d(r) } }
  case '{ ($l: Dual) * ($r: Dual) } =>
    '{ $l.v * ${ d(r) } + ${ d(l) } * $r.v }
  case '{ $varOrConst: Dual } => '{ $varOrConst.d }

inline def macroDifferentiate(inline d: Dual => Dual): Double => Double =
  ${diff('d)}
