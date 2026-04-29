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

import anticipation.*

object Error:
  def apply(throwable: Throwable): Error = throwable match
    case error: Error         => error
    case throwable: Throwable => UncheckedError(throwable)

transparent abstract class Error(val realm: Realm, val d: Int, val e: Int)
  ( val message: Message, private val cause: Throwable | Null )
  ( using val diagnostics: Diagnostics )
extends Exception(message.text.s, cause, false, diagnostics.captureStack):
  this: Error =>

  def this(realm: Realm, d: Int, e: Int)(message: Message)(using Diagnostics) =
    this(realm, d, e)(message, null)

  def this(message: Message, cause: Throwable | Null = null)(using Diagnostics) =
    this(Realm("zz"), 0, 0)(message, cause)

  def fullClass: List[Text] = List(getClass.getName.nn.split("\\.").nn.map(_.nn).map(Text(_))*)
  def className: Text = fullClass.last
  def component: Text = fullClass.head

  def errorCode: Text =
    if d == 0 then "".tt
    else
      val ePart = if e == 0 then "" else s".$e"
      s"SN-${realm.code}/$d$ePart".tt

  def colourCode: Text =
    if d == 0 then "".tt
    else
      val hyperlink = false
      val esc = 27.toChar
      val bel = 7.toChar
      val gray   = s"$esc[38;2;128;128;128m"
      val orange = s"$esc[38;2;255;165;0m"
      val yellow = s"$esc[38;2;255;215;0m"
      val cyan   = s"$esc[38;2;0;200;255m"
      val reset  = s"$esc[0m"
      val ePart  = if e == 0 then "" else s"$gray.$cyan$e"
      val link   = if hyperlink then s"$esc]8;;https://soundness.dev/SN-${realm.code}/$d$bel" else ""
      val unlink = if hyperlink then s"$esc]8;;$bel" else ""
      s"$link$gray[$orange↯SN$gray-$yellow${realm.code}$gray/$cyan$d$ePart$gray]$reset$unlink"
      .tt

  def labelled: Message =
    if d == 0 then message
    else m"[↯$errorCode] $message"

  override def getMessage: String =
    if d == 0 then component.s+": "+message.text
    else "[↯"+errorCode+"] "+message.text

  override def getCause: Throwable | Null = cause
