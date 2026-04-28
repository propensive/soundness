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
package yossarian

import anticipation.*
import fulminate.*

object PtyEscapeError:
  object Reason:
    given communicable: Reason is Communicable =
      case BadSgrParameters(ns)         => m"${ns} is not a valid SGR parameter sequence"
      case NonintegerSgrParameter(text) => m"$text is not a numerical SGR parameter"
      case BadColor(n)                  => m"$n is not a valid color number"
      case BadOscParameter(parameter)   => m"$parameter is not a recognized OSC parameter"
      case BadCsiEscape(char)           => m"$char is not valid in a CSI escape sequence"
      case BadFeEscape(char)            => m"$char is not a valid Fe escape"

      case BadCsiCommand(param, char) =>
        m"$char (with parameter $param) is not a valid CSI command"

      case BadCsiParameter(n, command) =>
        m"$n is not a valid CSI parameter for the $command command"

  enum Reason(val number: Int) extends Clarification:
    case BadSgrParameters(n: Text)              extends Reason(1)
    case BadCsiParameter(n: Int, command: Text) extends Reason(2)
    case NonintegerSgrParameter(text: Text)     extends Reason(3)
    case BadColor(n: Int)                       extends Reason(4)
    case BadOscParameter(parameter: Text)       extends Reason(5)
    case BadCsiCommand(param: Text, char: Char) extends Reason(6)
    case BadCsiEscape(char: Char)               extends Reason(7)
    case BadFeEscape(char: Char)                extends Reason(8)

case class PtyEscapeError(reason: PtyEscapeError.Reason)(using Diagnostics)
extends Error(realm"yo", 1, reason.number)
  ( m"an ANSI escape code could not be handled because $reason" )
