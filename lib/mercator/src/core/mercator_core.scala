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
package mercator

import anticipation.*
import murmuration.{Iterator, Reshapable, Traversable}
import prepositional.*

extension [value, functor[_]](using functor: Functor[functor])(value: functor[value])
  def map[value2](lambda: value => value2): functor[value2] =
    functor.map(value)(lambda)

extension [value, monad[_]](using monad: Monad[monad])(value: monad[value])
  def bind[value2](lambda: value => monad[value2]): monad[value2] =
    monad.bind(value)(lambda)

extension (text: Text)
  def bind(lambda: Char => Text): Text =
    val builder: StringBuilder = StringBuilder()

    text.s.toCharArray.nn.foreach: char => builder.append(lambda(char).s)

    builder.toString.tt

// Over any `Traversable`, rebuilt in the source's own shape through `Reshapable`: a `List` of
// `Try`s sequences to a `Try` of a `List`, a `Set` to a `Try` of a `Set`. The elements are
// gathered, reversed, as a `List` inside the monad, and reshaped once at the end.
extension [self](self: self)(using traversable: self is Traversable)
  def sequence[monad[_], element, result]
    ( using operand:    traversable.Operand <:< monad[element],
            monad:      Monad[monad],
            reshapable: self is Reshapable by element to result )
  :   monad[result] =

    traversable.traverse(self)
    . fold(monad.point(List.empty[element])): (accumulator, next) =>
        accumulator.flatMap { elements => operand(next).map(_ :: elements) }
    . map { elements => reshapable.reshape(Iterator.from(List.iterator(List.invert(elements)))) }


  def traverse[element2, monad[_], result](lambda: traversable.Operand => monad[element2])
    ( using monad: Monad[monad], reshapable: self is Reshapable by element2 to result )
  :   monad[result] =

    traversable.traverse(self)
    . fold(monad.point(List.empty[element2])): (accumulator, next) =>
        accumulator.flatMap { elements => lambda(next).map(_ :: elements) }
    . map { elements => reshapable.reshape(Iterator.from(List.iterator(List.invert(elements)))) }
