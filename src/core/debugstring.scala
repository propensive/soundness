/*
    Gossamer, version 0.5.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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
    xs => xs.map(summon[DebugString[T]].show(_)).join(t"Seq(", t", ", t")")

object DebugString extends Derivation[DebugString], FallbackDebugString:
  def join[T](ctx: CaseClass[DebugString, T]): DebugString[T] = t =>
    ctx.params.map {
      param => Text(param.label+" = "+param.typeclass.show(param.deref(t)))
    }.join(t"${ctx.typeInfo.short}(", t", ", t")")
  
  def split[T](ctx: SealedTrait[DebugString, T]): DebugString[T] = t =>
    ctx.choose(t) { subtype => subtype.typeclass.show(subtype.cast(t)) }

  given string: DebugString[Text] = x => Text("\""+x.flatMap {
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
    ch => t"'${string.show(ch.show).drop(1).dropRight(1)}'"

  given int: DebugString[Int] = _.show
  given long: DebugString[Long] = x => t"${x.show}L"
  given short: DebugString[Short] = x => t"${x.show}.toShort"
  given byte: DebugString[Byte] = x => t"${x.show}.toByte"
  given boolean: DebugString[Boolean] = _.show
  
  given double: DebugString[Double] = d =>
    if d != d then t"Double.NaN"
    else if d.isInfinite then t"Double.${if d < 0.0 then t"Negative" else t"Positive"}Infinity"
    else Text(d.toString)
  
  given float: DebugString[Float] = f =>
    if f != f then t"Float.NaN"
    else if f.isInfinite then t"Float.${if f < 0.0f then "Negative" else "Positive"}Infinity"
    else Text(f.toString+"F")

  val debugAny: DebugString[Any] = any => Text(any.toString)

trait DebugString[-T]:
  def show(value: T): Text

extension [T](value: T)
  def debug(using debugString: DebugString[T] = DebugString.debugAny): Text =
    debugString.show(value)