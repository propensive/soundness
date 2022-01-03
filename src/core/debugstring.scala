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
import rudiments.*

import scala.compiletime.*

trait FallbackDebugString:
  object Generic extends Derivation[Generic]:
    inline given [T]: Generic[T] = value => summonFrom:
      case given DebugString[T] => summon[DebugString[T]].show(value)
      case given Show[T]        => summon[Show[T]].show(value)
      case other: Any           => Showable(other).show
    
    def join[T](ctx: CaseClass[Generic, T]): Generic[T] = value => ctx.params.map:
      param => t"${param.label} = ${param.typeclass.show(param.deref(value))}"
    .join(t"${ctx.typeInfo.short}(", t", ", t")")
    
    def split[T](ctx: SealedTrait[Generic, T]): Generic[T] = t => ctx.choose(t):
      subtype => subtype.typeclass.show(subtype.cast(t))

  trait Generic[-T]:
    def show(value: T): Text

  given [T](using gen: Generic[T]): DebugString[T] = gen.show(_)

object DebugString extends FallbackDebugString:
  // given coll[Coll[X] <: Seq[X], T: DebugString]: DebugString[Coll[T]] =
  //   xs => xs.map(summon[DebugString[T]].show(_)).join(t"Seq(", t", ", t")")

  val showAny: DebugString[Any] = Showable(_).show

  inline given [T: DebugString]: DebugString[List[T]] = _.map(_.debug).join(t"List(", t", ", t")")

  given string: DebugString[Text] = text =>
    val escaped = text.flatMap:
      case '\n' => t"\\n"
      case '\t' => t"\\t"
      case '\r' => t"\\r"
      case '\\' => t"\\\\"
      case '\"' => t"\\\""
      case '\'' => t"\\\'"
      case '\b' => t"\\b"
      case '\f' => t"\\f"
      case ch   => if ch < 128 && ch >= 32 then ch.show
                   else String.format("\\u%04x", ch.toInt).nn.show

    t"\"$escaped\""

  given char: DebugString[Char] =
    ch => t"'${string.show(ch.show).drop(1).drop(1, Rtl)}'"

  given DebugString[Int] = _.show
  given DebugString[Long] = long => t"${long.show}L"
  given DebugString[Short] = short => t"${short.show}.toShort"
  given DebugString[Byte] = byte => t"${byte.show}.toByte"
  given DebugString[Boolean] = _.show
  
  given DebugString[Double] = double =>
    if double != double then t"Double.NaN"
    else if java.lang.Double.isInfinite(double)
    then t"Double.${if double < 0.0 then t"Negative" else t"Positive"}Infinity"
    else Showable(double).show
  
  given DebugString[Float] = float =>
    if float != float then t"Float.NaN"
    else if java.lang.Float.isInfinite(float)
    then t"Float.${if float < 0.0f then t"Negative" else t"Positive"}Infinity"
    else t"${Showable(float).show}F"
  
  inline def of[T]: DebugString[T] = (value: T) => summonFrom:
    case given DebugString[T] => summon[DebugString[T]].show(value)
    case given Show[T]        => summon[Show[T]].show(value)
    case _                    => Showable(value).show

trait DebugString[-T]:
  def show(value: T): Text

extension [T](value: T)
  inline def debug: Text = DebugString.of[T].show(value)