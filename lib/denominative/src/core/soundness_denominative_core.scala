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
package soundness

export
  denominative
  . { aka, capped, Countable, Vacuiscible, Applicable, Definable, Omissible, Terminable, Truncable, size, gamut, Interval, extent, iterate, prefix, nil, Ordinal, pare, Prim,
      Quat, Quin, retrace, Sec, Sen, Sept, Span, spot, Ter, u, z, Zerary, limit, ult, ant, pen,
      Dysasymptotic, beyond }

// `thru` and `till` are re-declared here rather than exported: an export forwards only one of
// the two overloads, so a plain `Ordinal` receiver would be offered the branded alternative
// and fail. Declaring both in one compilation unit restores ordinary overload resolution.
extension (ordinal: denominative.Ordinal)
  inline infix def thru (right: denominative.Ordinal): denominative.Interval =
    denominative.thru(ordinal)(right)

  inline infix def till (right: denominative.Ordinal): denominative.Interval =
    denominative.till(ordinal)(right)

extension [form](ordinal: prepositional.`in`[denominative.Ordinal, form])
  inline infix def thru (right: prepositional.`in`[denominative.Ordinal, form])
  :   prepositional.`in`[denominative.Interval, form] =
    denominative.thru(ordinal)(right)

  inline infix def till (right: prepositional.`in`[denominative.Ordinal, form])
  :   prepositional.`in`[denominative.Interval, form] =
    denominative.till(ordinal)(right)

package dysasymptotics:
  export denominative.dysasymptotics.{linearSize, linearAccess, unboundedSize}

package ordinalTextualizables:
  export
    denominative.ordinalTextualizables
    . { englishOrdinal, englishSuperscriptOrdinal, frenchOrdinal, intermediateOrdinal,
        italianOrdinal, nominalOrdinal, russianOrdinal, spanishOrdinal, uniaryOrdinal,
        unmarkedUniaryOrdinal, unmarkedZeraryOrdinal, zeraryOrdinal }
