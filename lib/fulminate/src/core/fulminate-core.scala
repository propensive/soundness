                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package fulminate

import language.experimental.captureChecking

import scala.compiletime.*
import scala.quoted.*

import anticipation.*

export Fulminate.Diagnostics

package errorDiagnostics:
  given empty: Diagnostics = Diagnostics.omit
  given stackTraces: Diagnostics = Diagnostics.capture

def panic(message: Message): Nothing = throw Panic(message)

def halt(using Quotes)
   (message: Message,
    pos: quotes.reflect.Position | Null = quotes.reflect.Position.ofMacroExpansion)
   (using Realm)
:     Nothing =
  import quotes.reflect.*
  import dotty.tools.dotc.config.Settings.Setting.value

  val useColor: Boolean = quotes match
    case quotes: runtime.impl.QuotesImpl =>
      value(quotes.ctx.settings.color)(using quotes.ctx) != "never"

    case _ =>
      false

  val esc = 27.toChar

  val text =
    if useColor then s"$esc[38;2;0;190;255m$esc[1m${summon[Realm].name}$esc[0m ${message.colorText}"
    else s"${summon[Realm].name}: ${message.text}"

  if pos == null then report.errorAndAbort(text) else report.errorAndAbort(text, pos)

def warn(using Quotes)(message: Message, pos: quotes.reflect.Position | Null = null)(using Realm)
:     Unit =
  import quotes.reflect.*
  import dotty.tools.dotc.config.Settings.Setting.value

  val esc = 27.toChar

  val useColor: Boolean = quotes match
    case quotes: runtime.impl.QuotesImpl =>
      value(quotes.ctx.settings.color)(using quotes.ctx) != "never"

    case _ =>
      false

  val text =
    if useColor
    then s"$esc[38;2;0;190;255m$esc[1m${summon[Realm].name}$esc[0m ${message.colorText}"
    else s"${summon[Realm].name}: ${message.text}"

  if pos == null then report.warning(text) else report.warning(text, pos)

extension (inline context: StringContext)
  transparent inline def m[ParamType](inline subs: ParamType = EmptyTuple): Message =
    inline subs.asMatchable match
      case tuple: Tuple =>
        import unsafeExceptions.canThrowAny

        Message
          (context.parts.map(_.tt).map(TextEscapes.escape(_)).to(List),
           Message.make[tuple.type](tuple, Nil))

      case other =>
        import unsafeExceptions.canThrowAny

        Message
          (context.parts.map(_.tt).map(TextEscapes.escape(_)).to(List),
           List(summonInline[(? >: other.type) is Communicable].message(other)))

extension (inline context: StringContext)
  inline def realm(): Realm = ${Fulminate.realm('context)}

extension [ValueType: Communicable](value: ValueType)
  def communicate: Message = ValueType.message(value)

inline def express[ErasedType]: Message = summonInline[ErasedType is Expressible].express()
