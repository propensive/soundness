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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import proscenium.*

export fulminate.internal.Diagnostics

package errorDiagnostics:
  given empty: Diagnostics = Diagnostics.omit
  given stackTraces: Diagnostics = Diagnostics.capture

def panic(message: Message): Nothing = throw Panic(message)

private def errorPrefix(realm: Realm, d: Int, e: Int, useColor: Boolean): String =
  val esc = 27.toChar
  if useColor then
    val gray   = s"$esc[38;2;128;128;128m"
    val orange = s"$esc[38;2;255;165;0m"
    val yellow = s"$esc[38;2;255;215;0m"
    val cyan   = s"$esc[38;2;0;200;255m"
    val reset  = s"$esc[0m"
    val ePart  = if e == 0 then "" else s"$gray.$cyan$e"
    s"$gray[$orange↯SN$gray-$yellow${realm.code}$gray/$cyan$d$ePart$gray]$reset "
  else
    val ePart = if e == 0 then "" else s".$e"
    s"[↯SN-${realm.code}/$d$ePart] "

private def detectColor(using quotes: Quotes): Boolean =
  import dotty.tools.dotc.config.Settings.Setting.value
  quotes match
    case quotes: runtime.impl.QuotesImpl =>
      value(quotes.ctx.settings.color)(using quotes.ctx) != "never"
    case _ =>
      false

def halt(using Quotes)(message: Message, position: quotes.reflect.Position | Null = null)
  ( using Realm )
:   Nothing =

  import quotes.reflect.*
  val text = if detectColor then message.colorText.s else message.text.s
  if position == null then report.errorAndAbort(text) else report.errorAndAbort(text, position)


def halt(using Quotes)(d: Int, message: Message)(using Realm): Nothing =
  haltImpl(message, null, errorPrefix(summon[Realm], d, 0, detectColor))

def halt(using Quotes)(d: Int, message: Message, position: quotes.reflect.Position | Null)
  (using Realm)
:   Nothing =
  haltImpl(message, position, errorPrefix(summon[Realm], d, 0, detectColor))

def halt(using Quotes)(d: Int, reason: Clarification, message: Message)(using Realm): Nothing =
  haltImpl(message, null, errorPrefix(summon[Realm], d, reason.number, detectColor))

def halt(using Quotes)
  (d: Int, reason: Clarification, message: Message, position: quotes.reflect.Position | Null)
  (using Realm)
:   Nothing =
  haltImpl(message, position, errorPrefix(summon[Realm], d, reason.number, detectColor))

private def haltImpl(using quotes: Quotes)
  (message: Message, position: quotes.reflect.Position | Null, prefix: String)
:   Nothing =

  import quotes.reflect.*

  val text = prefix+(if detectColor then message.colorText.s else message.text.s)
  if position == null then report.errorAndAbort(text) else report.errorAndAbort(text, position)


def warn(using Quotes)(message: Message, position: quotes.reflect.Position | Null = null)
  ( using Realm )
:   Unit =

  import quotes.reflect.*
  val text = if detectColor then message.colorText.s else message.text.s
  if position == null then report.warning(text) else report.warning(text, position)


def warn(using Quotes)(d: Int, message: Message)(using Realm): Unit =
  warnImpl(message, null, errorPrefix(summon[Realm], d, 0, detectColor))

def warn(using Quotes)(d: Int, message: Message, position: quotes.reflect.Position | Null)
  (using Realm)
:   Unit =
  warnImpl(message, position, errorPrefix(summon[Realm], d, 0, detectColor))

def warn(using Quotes)(d: Int, reason: Clarification, message: Message)(using Realm): Unit =
  warnImpl(message, null, errorPrefix(summon[Realm], d, reason.number, detectColor))

def warn(using Quotes)
  (d: Int, reason: Clarification, message: Message, position: quotes.reflect.Position | Null)
  (using Realm)
:   Unit =
  warnImpl(message, position, errorPrefix(summon[Realm], d, reason.number, detectColor))

private def warnImpl(using quotes: Quotes)
  (message: Message, position: quotes.reflect.Position | Null, prefix: String)
:   Unit =

  import quotes.reflect.*

  val text = prefix+(if detectColor then message.colorText.s else message.text.s)
  if position == null then report.warning(text) else report.warning(text, position)


extension (inline context: StringContext)
  transparent inline def m[param](inline subs: param = Zero): Message =
    inline subs.asMatchable match
      case tuple: Tuple =>
        import unsafeExceptions.canThrowAny

        Message
          ( context.parts.map(_.tt).map(TextEscapes.escape(_)).to(List),
            Message[tuple.type](tuple, Nil) )

      case other =>
        import unsafeExceptions.canThrowAny

        Message
          ( context.parts.map(_.tt).map(TextEscapes.escape(_)).to(List),
            List(infer[(? >: other.type) is Communicable].message(other)) )

extension (inline context: StringContext)
  inline def realm(): Realm = ${fulminate.internal.realm('context)}

extension [communicable: Communicable](value: communicable)
  def communicate: Message = communicable.message(value)
