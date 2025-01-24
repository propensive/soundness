/*
    Escapade, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import anticipation.*
import digression.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.experimental.captureChecking

object Teletypeable:
  given Teletype is Teletypeable = identity(_)
  given Text is Teletypeable = text => Teletype(text)
  given Pid is Teletypeable = pid => e"${pid.value.show}"

  given [ValueType: {Showable as showable, Colorable as colorable}] => ValueType is Teletypeable =
    value => e"${value.color}(${value.show})"

  given Message is Teletypeable = _.fold[Teletype](e""): (acc, next, level) =>
    level match
      case 0 => e"$acc${Fg(0xefe68b)}($next)"
      case 1 => e"$acc$Italic(${Fg(0xffd600)}($next))"
      case _ => e"$acc$Italic($Bold(${Fg(0xffff00)}($next)))"

  given [ValueType: Teletypeable] => Option[ValueType] is Teletypeable =
    case None        => Teletype("empty".show)
    case Some(value) => value.teletype

  given [ValueType: Showable] => ValueType is Teletypeable = value => Teletype(value.show)

  given TextMetrics => Exception is Teletypeable = exception =>
    summon[StackTrace is Teletypeable].teletype(StackTrace(exception))

  given Error is Teletypeable = _.message.teletype

  given TextMetrics => StackTrace is Teletypeable = stack =>
    val methodWidth = stack.frames.map(_.method.method.length).maxOption.getOrElse(0)
    val classWidth = stack.frames.map(_.method.className.length).maxOption.getOrElse(0)
    val fileWidth = stack.frames.map(_.file.length).maxOption.getOrElse(0)

    val fullClass = e"$Italic(${stack.component}.$Bold(${stack.className}))"
    val init = e"${Fg(0xffffff)}($fullClass): ${stack.message}"

    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.method.className.ends(t"#")
        val drop = if obj then 1 else 0
        val file = e"${Fg(0x5f9e9f)}(${frame.file.fit(fileWidth, Rtl)})"
        val dot = if obj then t"." else t"#"

        val className =
          e"${Fg(0xc61485)}(${frame.method.className.skip(drop, Rtl).fit(classWidth, Rtl)})"

        val method = e"${Fg(0xdb6f92)}(${frame.method.method.fit(methodWidth)})"
        val line = e"${Fg(0x47d1cc)}(${frame.line.let(_.show).or(t"?")})"

        e"$msg\n  ${Fg(0x808080)}(at) $className${Fg(0x808080)}($dot)$method $file${Fg(0x808080)}(:)$line"

    stack.cause.lay(root): cause =>
      e"$root\n${Fg(0xffffff)}(caused by:)\n$cause"

  given TextMetrics => StackTrace.Frame is Teletypeable = frame =>
    val className = e"${Fg(0xc61485)}(${frame.method.className.fit(40, Rtl)})"
    val method = e"${Fg(0xdb6f92)}(${frame.method.method.fit(40)})"
    val file = e"${Fg(0x5f9e9f)}(${frame.file.fit(18, Rtl)})"
    val line = e"${Fg(0x47d1cc)}(${frame.line.let(_.show).or(t"?")})"
    e"$className${Fg(0x808080)}(#)$method $file${Fg(0x808080)}(:)$line"

  given StackTrace.Method is Teletypeable = method =>
    val className = e"${Fg(0xc61485)}(${method.className})"
    val methodName = e"${Fg(0xdb6f92)}(${method.method})"
    e"$className${Fg(0x808080)}(#)$methodName"

  given (decimalizer: Decimalizer) => Double is Teletypeable = double =>
    Teletype.make(decimalizer.decimalize(double), _.copy(fg = 0xffd600))

  given Throwable is Teletypeable = throwable =>
    Teletype.make[String](throwable.getClass.getName.nn.show.cut(t".").last.s,
        _.copy(fg = 0xdc133b))

trait Teletypeable:
  type Self
  def teletype(value: Self): Teletype
