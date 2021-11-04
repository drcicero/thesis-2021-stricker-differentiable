package differentiable.reversemode.monadcps.functional.forless

import scala.quoted._
import scala.compiletime.{error}

inline def forless(inline m: Monad): Monad = ${ impl('m) }

private def impl(m: Expr[Monad])(using Quotes): Expr[Monad] = m match
  case '{
        val x = !($monadExpr: Monad)
        $tail(x): Monad
      } =>
    println("Matched monad")
    '{
      $monadExpr flatMap { x =>
        ${
          // Without betaReduce(...) the quoted code would be a function call and the recursive match would fail
          // E.g.: '{ {y => y * y}(x) } vs. '{ x * x }
          impl(Expr.betaReduce('{ $tail(x) }))
        }
      }
    }
  
  /*
   * val and def (and consequently var) must have their own cases because the 
   * 'head and tail' case doesn't match them somehow
   * sometimes produces an infinite compile loop: def f(v: Int) = v
  */

  case '{
        val v = ($vv: t)
        $tail(v): Monad
      } =>
    println("Matched val")
    impl(Expr.betaReduce('{ $tail($vv) }))
  /*
  case '{
        def f = ($fDef: t)
        $tail(f): Monad
      } =>
    println("Matched def")
    impl(Expr.betaReduce('{ $tail($fDef) }))
  */

  case '{
        $head: t // does not match val, var, def somehow
        $tail: Monad
      } =>
    println("Matched any")
    '{
      $head
      ${ impl(tail) }
    }

  case '{ ($x: Monad)._yield } =>
    println("Matched end")
    x

  case _ => 
//    println(m.show)
    m

//private def removeEx(n: Expr[Num])(using Quotes): Expr[Monad] = n match
//  case '{ !($mon: Monad) } => mon
