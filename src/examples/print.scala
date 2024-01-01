/*
    Wisteria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria.examples

import wisteria.*
import gossamer.*

// Prints a type, only requires read access to fields
trait Print[T] {
  def print(t: T): Text
}

trait GenericPrint extends Derivation[Print]:
  def join[T](ctx: CaseClass[Typeclass, T]): Print[T] = value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.print(param.deref(value))
    else ctx.params.map { param =>
      param.typeclass.print(param.deref(value))
    }.join(t"${ctx.typeInfo.short}(", t",", t")")

  override def split[T](ctx: SealedTrait[Print, T]): Print[T] =
    ctx.choose(_) { sub => sub.typeclass.print(sub.value) }

object Print extends GenericPrint:
  given Print[Text] = identity(_)
  given Print[Int] = _.show
  given seq[T](using printT: Print[T]): Print[Seq[T]] = _.map(printT.print).join(t"[", t",", t"]")
