package probably

import wisteria.*

extension [T: Approximable](value: T)
  def ~~(other: T): Boolean = summon[Approximable[T]].approximatelyEqual(value, other)

object Tolerance:
  given Tolerance[Double] = (a, b) => math.abs(a - b) <= math.pow(math.ulp((a + b)/2), 1/3.0)
  given Tolerance[Float] = (a, b) => math.abs(a - b) <= math.pow(math.ulp((a + b)/2), 1/3.0)

trait Tolerance[T]:
  def close(a: T, b: T): Boolean

trait Approximable[T]:
  def approximatelyEqual(a: T, b: T): Boolean

object Approximable extends Derivation[Approximable]:
  given Approximable[Boolean] = _ == _
  given Approximable[Byte] = _ == _
  given Approximable[Char] = _ == _
  given Approximable[Short] = _ == _
  given Approximable[Int] = _ == _
  given Approximable[Long] = _ == _
  given Approximable[String] = _ == _
  given [T: Tolerance]: Approximable[T] = summon[Tolerance[T]].close(_, _)

  def join[T](ctx: CaseClass[Approximable, T]): Approximable[T] = (v1, v2) =>
    ctx.params.forall { p => p.typeclass.approximatelyEqual(p.dereference(v1), p.dereference(v2)) }
  
  def split[T](ctx: SealedTrait[Approximable, T]): Approximable[T] = (v1, v2) =>
    ctx.split(v1) { sub => sub.typeclass.approximatelyEqual(sub.cast(v1), sub.cast(v2)) }
