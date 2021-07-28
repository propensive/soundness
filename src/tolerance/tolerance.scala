/*
    Probably, version 0.8.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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
    ctx.params.forall { p => p.typeclass.approximatelyEqual(p.deref(v1), p.deref(v2)) }
  
  def split[T](ctx: SealedTrait[Approximable, T]): Approximable[T] = (v1, v2) =>
    ctx.choose(v1) { sub => sub.typeclass.approximatelyEqual(sub.cast(v1), sub.cast(v2)) }
