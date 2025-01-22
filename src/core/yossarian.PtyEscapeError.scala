/*
    Yossarian, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package yossarian

import anticipation.*
import fulminate.*

object PtyEscapeError:
  object Reason:
    given Reason is Communicable as communicable =
      case BadSgrParameters(ns)         => m"${ns} is not a valid SGR parameter sequence"
      case BadCsiParameter(n, command)  => m"$n is not a valid CSI parameter for the $command command"
      case NonintegerSgrParameter(text) => m"$text is not a numerical SGR parameter"
      case BadColor(n)                  => m"$n is not a valid color number"
      case BadOscParameter(parameter)   => m"$parameter is not a recognized OSC parameter"
      case BadCsiCommand(param, char)   => m"$char (with parameter $param) is not a valid CSI command"
      case BadCsiEscape(char)           => m"$char is not valid in a CSI escape sequence"
      case BadFeEscape(char)            => m"$char is not a valid Fe escape"

  enum Reason:
    case BadSgrParameters(n: Text)
    case BadCsiParameter(n: Int, command: Text)
    case NonintegerSgrParameter(text: Text)
    case BadColor(n: Int)
    case BadOscParameter(parameter: Text)
    case BadCsiCommand(param: Text, char: Char)
    case BadCsiEscape(char: Char)
    case BadFeEscape(char: Char)

case class PtyEscapeError(reason: PtyEscapeError.Reason)(using Diagnostics)
extends Error(m"an ANSI escape code could not be handled because $reason")
