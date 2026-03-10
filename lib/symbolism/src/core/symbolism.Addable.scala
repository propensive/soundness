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
package symbolism

import prepositional.*

object Addable:
  def apply[augend, addend, result](lambda: (augend, addend) -> result)
  :   augend is Addable by addend to result =

    new Addable:

      type Self = augend
      type Result = result
      type Operand = addend

      def add(augend: augend, addend: addend): result = lambda(augend, addend)


  given double: Double is Addable by Double to Double = Addable:
    (augend, addend) => augend + addend

  given float: Float is Addable by Float to Float = Addable:
    (augend, addend) => augend + addend

  given long: Long is Addable by Long to Long = Addable:
    (augend, addend) => augend + addend

  given int: Int is Addable by Int to Int = Addable:
    (augend, addend) => augend + addend

  given int2: Int is Addable by Double to Double = Addable:
    (augend, addend) => augend + addend

  given int3: Double is Addable by Int to Double = Addable:
    (augend, addend) => augend + addend

  given short: Short is Addable by Short to Short = Addable:
    (augend, addend) => (augend + addend).toShort

  given byte: Byte is Addable by Byte to Byte = Addable:
    (augend, addend) => (augend + addend).toByte


  given concatenable: [left, right] => (concatenable: left is Concatenable by right)
  =>  left is Addable:

    type Self = left
    type Operand = right
    type Result = left | right

    def add(left: left, right: right): left | right = concatenable.concat(left, right)


trait Addable extends Typeclass, Operable, Resultant:
  type Augend = Self
  type Addend = Operand

  def add(augend: Augend, addend: Addend): Result
