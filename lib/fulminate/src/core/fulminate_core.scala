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
    val hyperlink = false
    val bel = 7.toChar
    val gray   = s"$esc[38;2;128;128;128m"
    val orange = s"$esc[38;2;255;165;0m"
    val yellow = s"$esc[38;2;255;215;0m"
    val cyan   = s"$esc[38;2;0;200;255m"
    val reset  = s"$esc[0m"
    val ePart  = if e == 0 then "" else s"$gray.$cyan$e"
    val link   = if hyperlink then s"$esc]8;;https://soundness.dev/SN-${realm.code}/$d$bel" else ""
    val unlink = if hyperlink then s"$esc]8;;$bel" else ""
    s"$link$gray[$orange↯SN$gray-$yellow${realm.code}$gray/$cyan$d$ePart$gray]$reset$unlink "
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


def halt(d: Int, message: Message)(using quotes: Quotes, realm: Realm): Nothing =
  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.errorAndAbort(errorPrefix(realm, d, 0, detectColor)+body)


def halt(d: Int, message: Message, position: Matchable)(using quotes: Quotes, realm: Realm)
:   Nothing =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(realm, d, 0, detectColor)+body
  position match
    case null                     => report.errorAndAbort(text)
    case pos: Position @unchecked => report.errorAndAbort(text, pos)
    case _                        => report.errorAndAbort(text)


def halt(d: Int, reason: Clarification, message: Message)(using quotes: Quotes, realm: Realm)
:   Nothing =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.errorAndAbort(errorPrefix(realm, d, reason.number, detectColor)+body)


def halt(d: Int, reason: Clarification, message: Message, position: Matchable)
  ( using quotes: Quotes, realm: Realm )
:   Nothing =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(realm, d, reason.number, detectColor)+body
  position match
    case null                     => report.errorAndAbort(text)
    case pos: Position @unchecked => report.errorAndAbort(text, pos)
    case _                        => report.errorAndAbort(text)


def warn(using Quotes)(message: Message, position: quotes.reflect.Position | Null = null)
  ( using Realm )
:   Unit =

  import quotes.reflect.*
  val text = if detectColor then message.colorText.s else message.text.s
  if position == null then report.warning(text) else report.warning(text, position)


def warn(d: Int, message: Message)(using quotes: Quotes, realm: Realm): Unit =
  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.warning(errorPrefix(realm, d, 0, detectColor)+body)


def warn(d: Int, message: Message, position: Matchable)(using quotes: Quotes, realm: Realm)
:   Unit =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(realm, d, 0, detectColor)+body

  position match
    case null                     => report.warning(text)
    case pos: Position @unchecked => report.warning(text, pos)
    case _                        => report.warning(text)


def warn(d: Int, reason: Clarification, message: Message)
  (using quotes: Quotes, realm: Realm)
:   Unit =
  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.warning(errorPrefix(realm, d, reason.number, detectColor)+body)


def warn(d: Int, reason: Clarification, message: Message, position: Matchable)
  ( using quotes: Quotes, realm: Realm )
:   Unit =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(realm, d, reason.number, detectColor)+body
  position match
    case null                     => report.warning(text)
    case pos: Position @unchecked => report.warning(text, pos)
    case _                        => report.warning(text)


extension (inline context: StringContext)
  transparent inline def m[param](inline subs: param = Zero): Message =
    ${ fulminate.internal.mMacro[param]('context, 'subs) }

extension (inline context: StringContext)
  inline def realm(): Realm = ${fulminate.internal.realm('context)}

extension [communicable: Communicable](value: communicable)
  def communicate: Message = communicable.message(value)
