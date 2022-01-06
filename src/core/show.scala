/*
    Gossamer, version 0.5.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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

trait Show[-T]:
  def show(value: T): Text

object Show extends Derivation[Show]:
  given Show[String] = Text(_)
  given Show[Text] = identity(_)
  given Show[Int] = Showable(_).show
  given Show[Short] = Showable(_).show
  given Show[Long] = Showable(_).show
  given Show[Byte] = Showable(_).show
  
  given Show[ByteSize] = bs =>
    if bs.long > 10L*1024*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024*1024).show}TB")
    else if bs.long > 10L*1024*1024*1024 then Text(s"${(bs.long/1024*1024*1024).show}GB")
    else if bs.long > 10*1024*1024 then Text(s"${bs.long/1024*1024}MB")
    else if bs.long > 10*1024 then Text(s"${bs.long/1024}kB")
    else Text(s"${bs.long}B")
  
  given Show[Char] = Showable(_).show
  given Show[Boolean] = if _ then Text("true") else Text("false")
  given Show[reflect.Enum] = Showable(_).show
  
  given [T: Show]: Show[Option[T]] =
    case None    => Text("none")
    case Some(v) => v.show

  def join[T](ctx: CaseClass[Show, T]): Show[T] = value =>
    if ctx.isObject then Text(ctx.typeInfo.short)
    else ctx.params.map {
      param => param.typeclass.show(param.deref(value))
    }.join(Text(s"${ctx.typeInfo.short}("), Text(", "), Text(")"))
  
  def split[T](ctx: SealedTrait[Show, T]): Show[T] = value =>
    ctx.choose(value) { subtype => subtype.typeclass.show(subtype.cast(value)) }

extension [T](value: T) def show(using show: Show[T]): Text = show.show(value)