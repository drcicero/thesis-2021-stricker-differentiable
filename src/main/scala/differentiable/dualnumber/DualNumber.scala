package differentiable.dualnumber

case class Dual(v: Double, d: Double):
  def *(that: Dual) = Dual(
    this.v * that.v,
    this.v * that.d + this.d * that.v
  )

  def +(that: Dual) = Dual(
    this.v + that.v,
    this.d + that.d
  )

def const(v: Double) = Dual(v, 0)
def variable(v: Double) = Dual(v, 1)

@main def mainDualNumber() =
  def differentiate(f: Dual => Dual)(x: Double): Double =
    val result: Dual = f(variable(x))
    result.d // result.v would be the actual result of f(x)

  def f(x: Dual): Dual = const(2) * x + x * x * x

  println(differentiate(f)(3))