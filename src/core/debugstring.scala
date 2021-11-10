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

import scala.compiletime.*

trait FallbackDebugString:
  object Generic extends Derivation[Generic]:
    inline given [T]: Generic[T] = value => summonFrom {
      case given DebugString[T] => summon[DebugString[T]].show(value)
      case given Show[T]        => summon[Show[T]].show(value)
      case other: Any           => Showable(other).show
    }
    
    def join[T](ctx: CaseClass[Generic, T]): Generic[T] = t =>
      ctx.params.map { param =>
        t"${param.label} = ${param.typeclass.show(param.deref(t))}"
      }.join(t"${ctx.typeInfo.short}(", t", ", t")")
    
    def split[T](ctx: SealedTrait[Generic, T]): Generic[T] = t =>
      ctx.choose(t) { subtype => subtype.typeclass.show(subtype.cast(t)) }

  trait Generic[-T]:
    def show(value: T): Text

  given [T](using gen: Generic[T]): DebugString[T] = gen.show(_)

object DebugString extends FallbackDebugString:
  // given coll[Coll[X] <: Seq[X], T: DebugString]: DebugString[Coll[T]] =
  //   xs => xs.map(summon[DebugString[T]].show(_)).join(t"Seq(", t", ", t")")

  val showAny: DebugString[Any] = Showable(_).show

  inline given list[T: DebugString]: DebugString[List[T]] = _.map(_.debug).join(t"List(", t", ", t")")

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
    else Showable(d).show
  
  given float: DebugString[Float] = f =>
    if f != f then t"Float.NaN"
    else if f.isInfinite then t"Float.${if f < 0.0f then "Negative" else "Positive"}Infinity"
    else t"${Showable(f).show}F"
  
  inline def of[T]: DebugString[T] = (value: T) => summonFrom {
    case given DebugString[T] => summon[DebugString[T]].show(value)
    case given Show[T]        => summon[Show[T]].show(value)
    case _                    => Showable(value).show
  }

trait DebugString[-T]:
  def show(value: T): Text

extension [T](value: T)
  inline def debug: Text = DebugString.of[T].show(value)