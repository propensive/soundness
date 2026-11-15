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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.quoted.*

import anticipation.*

export fulminate.internal.Diagnostics

package errorDiagnostics:
  given emptyDiagnostics: Diagnostics = Diagnostics.omit
  given stackTracesDiagnostics: Diagnostics = Diagnostics.capture

def panic(message: Message): Nothing = throw Panic(message)

private def errorPrefix(module: Int, number: Int, variant: Int, useColor: Boolean): String =
  val bolt = Bolt(Bolt.Soundness, module, number, variant)
  (if useColor then bolt.colorLabel.s else bolt.label.s)+" "

private def detectColor(using quotes: Quotes): Boolean =
  import dotty.tools.dotc.config.Settings.Setting.value

  quotes match
    case quotes: runtime.impl.QuotesImpl =>
      value(quotes.ctx.settings.color)(using quotes.ctx) != "never"

    case _ =>
      false

def halt(using Quotes)(message: Message, position: quotes.reflect.Position | Null = null)
:   Nothing =

  import quotes.reflect.*
  val text = if detectColor then message.colorText.s else message.text.s
  if position == null then report.errorAndAbort(text) else report.errorAndAbort(text, position)


def halt(module: Int, number: Int, message: Message)(using quotes: Quotes): Nothing =
  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.errorAndAbort(errorPrefix(module, number, 0, detectColor)+body)

def halt(module: Int, number: Int, message: Message, position: Matchable)(using quotes: Quotes)
:   Nothing =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(module, number, 0, detectColor)+body

  position match
    case null                     => report.errorAndAbort(text)
    case pos: Position @unchecked => report.errorAndAbort(text, pos)
    case _                        => report.errorAndAbort(text)

def halt(module: Int, number: Int, reason: Clarification, message: Message)(using quotes: Quotes)
:   Nothing =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.errorAndAbort(errorPrefix(module, number, reason.number, detectColor)+body)


def halt(module: Int, number: Int, reason: Clarification, message: Message, position: Matchable)
  ( using quotes: Quotes )
:   Nothing =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(module, number, reason.number, detectColor)+body

  position match
    case null                     => report.errorAndAbort(text)
    case pos: Position @unchecked => report.errorAndAbort(text, pos)
    case _                        => report.errorAndAbort(text)


def warn(using Quotes)(message: Message, position: quotes.reflect.Position | Null = null)
:   Unit =

  import quotes.reflect.*
  val text = if detectColor then message.colorText.s else message.text.s
  if position == null then report.warning(text) else report.warning(text, position)


def warn(module: Int, number: Int, message: Message)(using quotes: Quotes): Unit =
  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.warning(errorPrefix(module, number, 0, detectColor)+body)

def warn(module: Int, number: Int, message: Message, position: Matchable)(using quotes: Quotes)
:   Unit =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(module, number, 0, detectColor)+body

  position match
    case null                     => report.warning(text)
    case pos: Position @unchecked => report.warning(text, pos)
    case _                        => report.warning(text)

def warn(module: Int, number: Int, reason: Clarification, message: Message)(using quotes: Quotes)
:   Unit =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  report.warning(errorPrefix(module, number, reason.number, detectColor)+body)


def warn(module: Int, number: Int, reason: Clarification, message: Message, position: Matchable)
  ( using quotes: Quotes )
:   Unit =

  import quotes.reflect.*
  val body = if detectColor then message.colorText.s else message.text.s
  val text = errorPrefix(module, number, reason.number, detectColor)+body

  position match
    case null                     => report.warning(text)
    case pos: Position @unchecked => report.warning(text, pos)
    case _                        => report.warning(text)


extension (inline context: StringContext)
  transparent inline def m[param](inline subs: param = Zero): Message =
    ${fulminate.internal.mMacro[param]('context, 'subs)}

extension [communicable: Communicable](value: communicable)
  def communicate: Message = communicable.message(value)
