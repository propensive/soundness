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
package mercator

import anticipation.*
import fulminate.*

import scala.collection.BuildFrom

private given realm: Realm = realm"mercator"

extension [value, functor[_]](using functor: Functor[functor])(value: functor[value])
  def map[value2](lambda: value => value2): functor[value2] =
    functor.map(value)(lambda)

extension [value, monad[_]](using monad: Monad[monad])(value: monad[value])
  def bind[value2](lambda: value => monad[value2]): monad[value2] =
    monad.flatMap(value)(lambda)

extension (text: Text)
  def bind(lambda: Char => Text): Text =
    val builder: StringBuilder = StringBuilder()
    text.s.toCharArray.nn.foreach: char =>
      builder.append(lambda(char).s)
    builder.toString.tt

extension [monad[_], collection[element] <: Iterable[element], element]
  ( elems: collection[monad[element]] )
          (using monad: Monad[monad])

  def sequence(using buildFrom: BuildFrom[List[element], element, collection[element]])
  : monad[collection[element]] =


      def recur(todo: Iterable[monad[element]], accumulator: monad[List[element]])
      : monad[List[element]] =

          if todo.isEmpty then accumulator
          else recur(todo.tail, accumulator.flatMap { xs => todo.head.map(_ :: xs) })


      recur(elems, monad.point(List())).map(_.reverse.to(buildFrom.toFactory(Nil)))


extension [collection[element] <: Iterable[element], element](elems: collection[element])
  def traverse[element2, monad[_]](lambda: element => monad[element2])
    ( using monad:     Monad[monad],
              buildFrom: BuildFrom[List[element2], element2, collection[element2]])
  : monad[collection[element2]] =


      def recur(todo: Iterable[element], accumulator: monad[List[element2]])
      : monad[List[element2]] =

          if todo.isEmpty then accumulator
          else recur(todo.tail, accumulator.flatMap { xs => lambda(todo.head).map(_ :: xs) })


      recur(elems, monad.point(List())).map(_.reverse.to(buildFrom.toFactory(Nil)))
