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
package rudiments

import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*
import prepositional.*
import proscenium.*
import symbolism.*
import vacuous.*

object internal:
  private given realm: Realm = realm"ru"

  opaque type Bytes = Long
  opaque type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

  @annotation.targetName("And")
  object `&`:
    def unapply[value](value: value): Some[(value, value)] = Some((value, value))

  object Digit:
    def apply(char: Char): Optional[Digit] = apply(char - '0')

    def apply(int: Int): Optional[Digit] = int match
      case digit: Digit => digit
      case _            => Unset


  extension (digit: Digit)
    def int: Int = digit

    def char: Char = ('0' + int).toChar


  object Bytes:
    def apply(long: Long): Bytes = long

    given ordering: Ordering[Bytes] = Ordering.Long.on(_.long)

    given zeroic: Bytes is Zeroic:
      inline def zero: Bytes = 0L

    given communicable: [bytes <: Bytes] => bytes is Communicable =
      bytes => Message(bytes.text)

    given addable: Bytes is Addable by Bytes to Bytes = _ + _
    given subtractable: Bytes is Subtractable by Bytes to Bytes = _ - _
    given multiplicable: Bytes is Multiplicable by Int to Bytes = _*_
    given multiplicable2: Int is Multiplicable by Bytes to Bytes = _*_
    given divisible: Bytes is Divisible by Int to Bytes = _/_
    given divisible2: Bytes is Divisible by Bytes to Double = _.toDouble/_


    extension (left: Bytes)
      def long: Long = left

      def text: Text = (left.toString+" bytes").tt

  def probe[target: Type]: Macro[Nothing] =
    import quotes.reflect.*
    halt(1, m"the type is ${TypeRepr.of[target].dealias.widen.show}")

  def name[target: Type]: Macro[Text] =
    import quotes.reflect.*

    val name = TypeRepr.of[target].show
    Expr[Text](name)

  def reflectClass[target: Type]: Macro[Class[target]] =
    import quotes.reflect.*
    '{Class.forName(${Expr(TypeRepr.of[target].typeSymbol.fullName)}).asInstanceOf[Class[target]]}
