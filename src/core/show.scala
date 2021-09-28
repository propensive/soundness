/*
    Gossamer, version 0.5.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import wisteria.*

trait Show[-T]:
  def show(value: T): Txt

object Show extends Derivation[Show]:
  given Show[String] = Txt(_)
  given Show[Int] = num => Txt(num.toString)
  given Show[Short] = num => Txt(num.toString)
  given Show[Long] = num => Txt(num.toString)
  given Show[Byte] = num => Txt(num.toString)
  given Show[Char] = ch => Txt(ch.toString)
  given Show[Boolean] = if _ then Txt("true") else Txt("false")

  def join[T](ctx: CaseClass[Show, T]): Show[T] = value =>
    if ctx.isObject then ctx.typeInfo.short.text
    else ctx.params.map {
      param => param.typeclass.show(param.deref(value)).s
    }.join(str"${ctx.typeInfo.short}(", ", ", ")").text
  
  def split[T](ctx: SealedTrait[Show, T]): Show[T] = value =>
    ctx.choose(value) { subtype => subtype.typeclass.show(subtype.cast(value)) }

extension [T](value: T) def show(using show: Show[T]) = show.show(value)