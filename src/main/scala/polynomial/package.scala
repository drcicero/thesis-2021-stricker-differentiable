package polynomial

import Polyinomial._
import compiletime.{erasedValue, constValue}
import compiletime.ops.int.-

sealed class Polyn
case class Mul[L <: Poly, R <: Poly](l: L, r: R) extends Polyn
case class Plus[L <: Poly, R <: Poly](l: L, r: R) extends Polyn
case class Pow[B <: Poly, N <: Int](b: B, n: N) extends Polyn
case class Var() extends Polyn
case class Val[C <: Double | Int](d: C) extends Polyn

object Polyinomial:
  type Poly = Polyn
  infix type *[L <: Poly, R <: Poly] = Mul[L, R]
  infix type +[L <: Poly, R <: Poly] = Plus[L, R]
  infix type ^[B <: Poly, N <: Int] = Pow[B, N]
  type X = Var
  type V[C <: Double | Int] = Val[C]

  type D[P] = P match
    case l * r => l match
      case V[0] => V[0]
      case V[_] => l * D[r]
      case _ => r match
        case V[0] => V[0]
        case V[_] => r * D[l]
        case _ => l * D[r] + D[l] * r

    case l + r => l match
      case V[_] => D[r]
      case _ => r match
        case V[_] => D[l]
        case _ => D[l] + D[r]

    case X ^ n => n match
      case 0 => V[0]
      case 1 => V[1]
      case _ => V[n] * (X^(n-1))

    case X => V[1]
    case V[c] => V[0]


inline def initPolynomial[P <: Poly]: P =
  val res =
    inline erasedValue[P] match
      case _: (l * r) => Mul(initPolynomial[l], initPolynomial[r])
      case _: (l + r) => Plus(initPolynomial[l], initPolynomial[r])
      case _: (b ^ n) => Pow(initPolynomial[b], constValue[n])
      case _: X => Var()
      case _: V[c] => Val(constValue[c])
  res.asInstanceOf[P]

inline def d[P <: Poly]: D[P] = initPolynomial