/*
    Escapade, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import language.experimental.pureFunctions

import anticipation.*
import fulminate.*

export Escapade.CharSpan

object Bold
object Italic
object Underline
object Strike
object Reverse
object Conceal

extension (inline ctx: StringContext)
  transparent inline def e(inline parts: Any*): Teletype = ${Ansi.Interpolator.expand('ctx, 'parts)}

extension [ValueType: Teletypeable](value: ValueType) def teletype: Teletype =
  ValueType.teletype(value)

package printableTypes:
  given Message is Printable as message = summon[Teletype is Printable].contramap(_.teletype)
