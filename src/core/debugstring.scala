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

trait FallbackDebugString:
  given fromShow[T: Show]: DebugString[T] = summon[Show[T]].show(_)

  given coll[Coll[X] <: Seq[X], T: DebugString]: DebugString[Coll[T]] =
    xs => xs.map(summon[DebugString[T]].show(_)).join(str"Seq(", str", ", str")")

object DebugString extends Derivation[DebugString], FallbackDebugString:
  def join[T](ctx: CaseClass[DebugString, T]): DebugString[T] = t =>
    ctx.params.map {
      param => Txt(param.label+" = "+param.typeclass.show(param.deref(t)))
    }.join(str"${ctx.typeInfo.short}(", str", ", str")")
  
  def split[T](ctx: SealedTrait[DebugString, T]): DebugString[T] = t =>
    ctx.choose(t) { subtype => subtype.typeclass.show(subtype.cast(t)) }

  given string: DebugString[Txt] = x => Txt("\""+x.flatMap {
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case '\\' => "\\\\"
    case '\"' => "\\\""
    case '\'' => "\\\'"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case ch   => if ch < 128 && ch >= 32 then ch.show else String.format("\\u%04x", ch.toInt).nn
  }+"\"")

  given char: DebugString[Char] =
    ch => str"'${string.show(ch.show).drop(1).dropRight(1)}'"

  given int: DebugString[Int] = _.show
  given long: DebugString[Long] = x => str"${x.show}L"
  given short: DebugString[Short] = x => str"${x.show}.toShort"
  given byte: DebugString[Byte] = x => str"${x.show}.toByte"
  given boolean: DebugString[Boolean] = _.show
  
  given double: DebugString[Double] = d =>
    if d != d then str"Double.NaN"
    else if d.isInfinite then str"Double.${if d < 0.0 then str"Negative" else str"Positive"}Infinity"
    else Txt(d.toString)
  
  given float: DebugString[Float] = f =>
    if f != f then str"Float.NaN"
    else if f.isInfinite then str"Float.${if f < 0.0f then "Negative" else "Positive"}Infinity"
    else Txt(f.toString+"F")

  val debugAny: DebugString[Any] = any => Txt(any.toString)

trait DebugString[-T]:
  def show(value: T): Txt

extension [T](value: T)
  def debug(using debugString: DebugString[T] = DebugString.debugAny): Txt =
    debugString.show(value)