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
package mercator

import proscenium.compat.*

import scala.collection.BuildFrom

import anticipation.*

extension [value, functor[_]](using functor: Functor[functor])(value: functor[value])
  def map[value2](lambda: value => value2): functor[value2] =
    functor.map(value)(lambda)

extension [value, monad[_]](using monad: Monad[monad])(value: monad[value])
  def bind[value2](lambda: value => monad[value2]): monad[value2] =
    monad.bind(value)(lambda)

extension (text: Text)
  def bind(lambda: Char => Text): Text =
    val builder: StringBuilder = StringBuilder()

    text.s.toCharArray.nn.foreach: char =>
      builder.append(lambda(char).s)

    builder.toString.tt

extension [monad[_], collection[element] <: Iterable[element],
  element](elems: collection[monad[element]])
  (using monad: Monad[monad])

  def sequence(using buildFrom: BuildFrom[scala.collection.immutable.List[element], element, collection[element]])
  :   monad[collection[element]] =


    def recur(todo: Iterable[monad[element]], accumulator: monad[scala.collection.immutable.List[element]])
    :   monad[scala.collection.immutable.List[element]] =

      if todo.isEmpty then accumulator
      else recur(todo.tail, accumulator.flatMap { xs => todo.head.map(x => x :: xs) })


    recur(elems, monad.point(scala.collection.immutable.List())).map(_.reverse.to(buildFrom.toFactory(scala.collection.immutable.List())))


extension [collection[element] <: Iterable[element], element](elems: collection[element])
  def traverse[element2, monad[_]](lambda: element => monad[element2])
    ( using monad:     Monad[monad],
            buildFrom: BuildFrom[scala.collection.immutable.List[element2], element2, collection[element2]] )
  :   monad[collection[element2]] =


    def recur(todo: Iterable[element], accumulator: monad[scala.collection.immutable.List[element2]])
    :   monad[scala.collection.immutable.List[element2]] =

      if todo.isEmpty then accumulator
      else recur(todo.tail, accumulator.flatMap { xs => lambda(todo.head).map(x => x :: xs) })


    recur(elems, monad.point(scala.collection.immutable.List())).map(_.reverse.to(buildFrom.toFactory(scala.collection.immutable.List())))
