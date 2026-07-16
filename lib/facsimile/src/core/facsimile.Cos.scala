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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package facsimile

import anticipation.*
import rudiments.*
import vacuous.*

object Cos:
  extension (cos: Cos)
    def dictionary: Optional[Map[Text, Cos]] = cos match
      case Cos.Dictionary(entries) => entries
      case Cos.Body(entries, _)    => entries // a stream is its dictionary for lookup purposes
      case _                       => Unset

    // Dictionary lookup: a `null` value is equivalent to an absent key (ISO 32000-2 §7.3.9),
    // so internal code deals only in `Optional`.
    def apply(key: Text): Optional[Cos] = dictionary.let(_.at(key)).let:
      case Cos.Nil => Unset
      case other   => other

    def long: Optional[Long] = cos match
      case Cos.Integral(value) => value
      case _                   => Unset

    // Numeric reads accept either numeric type; the parsed distinction is kept for writing.
    def double: Optional[Double] = cos match
      case Cos.Integral(value) => value.toDouble
      case Cos.Real(value)     => value
      case _                   => Unset

    def name: Optional[Text] = cos match
      case Cos.Name(text) => text
      case _              => Unset

    def truth: Optional[Boolean] = cos match
      case Cos.Truth(value) => value
      case _                => Unset

    def chars: Optional[Data] = cos match
      case Cos.Chars(bytes) => bytes
      case _                => Unset

    def elements: Optional[List[Cos]] = cos match
      case Cos.Sequence(elements) => elements
      case _                      => Unset

// The COS ("Carousel Object System") object model: the eight basic object types of
// ISO 32000-2 §7.3, plus streams and indirect references. `Chars` keeps a string's raw bytes:
// interpreting them (PDFDocEncoding, UTF-16BE, dates) is a semantic decision deferred to the
// typed document layer. `Body` is a stream *locator* — its dictionary plus the file offset of
// the raw payload — not the payload itself, which can only be dereferenced through a `Pdf`.
enum Cos:
  case Nil
  case Truth(value: Boolean)
  case Integral(value: Long)
  case Real(value: Double)
  case Name(text: Text)
  case Chars(bytes: Data)
  case Sequence(elements: List[Cos])
  case Dictionary(entries: Map[Text, Cos])
  case Body(entries: Map[Text, Cos], start: Long)
  case Ref(number: Int, generation: Int)
