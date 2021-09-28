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

object DebugString extends Derivation[DebugString]:
  given fromShow[T: Show]: DebugString[T] = summon[Show[T]].show(_).s

  def join[T](ctx: CaseClass[DebugString, T]): DebugString[T] = t =>
    ctx.params.map {
      param => param.label+" = "+param.typeclass.show(param.deref(t))
    }.join(str"${ctx.typeInfo.short}(", ", ", ")")
  
  def split[T](ctx: SealedTrait[DebugString, T]): DebugString[T] = t =>
    ctx.choose(t) { subtype => subtype.typeclass.show(subtype.cast(t)) }

  given coll[Coll[X] <: Seq[X], T: DebugString]: DebugString[Coll[T]] =
    xs => xs.map(summon[DebugString[T]].show(_)).join("Seq(", ", ", ")")

  given string: DebugString[String] = "\""+_.flatMap {
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case '\\' => "\\\\"
    case '\"' => "\\\""
    case '\'' => "\\\'"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case ch   => if ch < 128 && ch >= 32 then ch.toString else String.format("\\u%04x", ch.toInt).nn
  }+"\""

  given char: DebugString[Char] =
    ch => "'"+string.show(ch.toString).drop(1).dropRight(1)+"'"

  given int: DebugString[Int] = _.toString
  given long: DebugString[Long] = _.toString+"L"
  given short: DebugString[Short] = _.toString+".toShort"
  given byte: DebugString[Byte] = _.toString+".toByte"
  given boolean: DebugString[Boolean] = _.toString
  
  given double: DebugString[Double] = d =>
    if d != d then "Double.NaN"
    else if d.isInfinite then str"Double.${if d < 0.0 then "Negative" else "Positive"}Infinity"
    else d.toString
  
  given float: DebugString[Float] = f =>
    if f != f then "Float.NaN"
    else if f.isInfinite then str"Float.${if f < 0.0f then "Negative" else "Positive"}Infinity"
    else f.toString+"F"

  val debugAny: DebugString[Any] = _.toString

trait DebugString[-T]:
  def show(value: T): String

extension [T](value: T)
  def debug(using debugString: DebugString[T] =
    DebugString.debugAny): String = debugString.show(value)