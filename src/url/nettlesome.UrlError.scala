/*
    Nettlesome, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import anticipation.*
import denominative.*
import fulminate.*

case class UrlError(text: Text, offset: Ordinal, expected: UrlError.Expectation)(using Diagnostics)
extends Error(m"the URL $text is not valid: expected $expected at ${offset.n0}")

object UrlError:
  enum Expectation:
    case Colon, More, LowerCaseLetter, PortRange, Number

  object Expectation:
    given Expectation is Communicable =
      case Colon           => m"a colon"
      case More            => m"more characters"
      case LowerCaseLetter => m"a lowercase letter"
      case PortRange       => m"a port range"
      case Number          => m"a number"
