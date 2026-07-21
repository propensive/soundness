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
package murmuration

import scala.collection.immutable.IndexedSeq

import scala.reflect.ClassTag

import anticipation.*
import prepositional.*

// Conversion to a *requested shape*: `Form` names the target — a proper type (`Text`) or an
// unapplied constructor (`Map`, `List`) — and `Result` the fully-applied result implicit search
// derives for it. It is driven by the kind-polymorphic `to` extension: `pairs.to[Map]` yields a
// `Map[key, value]`, `chars.to[Text]` a `Text`. `Form` is declared `<: AnyKind` so one method
// serves both kinds, uniformly, unlike the stdlib's `Factory`-taking `to`.
object Convertible:
  given list: [self] => (traversable: self is Traversable)
  =>  self is Convertible in List to List[traversable.Operand] =
    self => List.from(traversable.traverse(self))

  given set: [self] => (traversable: self is Traversable)
  =>  self is Convertible in Set to Set[traversable.Operand] =
    self => Set.from(traversable.traverse(self))

  given series: [self] => (traversable: self is Traversable)
  =>  self is Convertible in Series to Series[traversable.Operand] =
    self => Series.from(traversable.traverse(self))

  given indexedSeq: [self] => (traversable: self is Traversable)
  =>  self is Convertible in IndexedSeq to IndexedSeq[traversable.Operand] =
    self => IndexedSeq.from(traversable.traverse(self))

  given lazyList: [self] => (traversable: self is Traversable)
  =>  self is Convertible in Progression to Progression[traversable.Operand] =
    self => Progression.from(traversable.traverse(self))

  given map: [self, key, value] => (traversable: self is Traversable by (key, value))
  =>  self is Convertible in Map to Map[key, value] =
    self => Map.from(traversable.traverse(self))

  given iarray: [self]
  =>  (traversable: self is Traversable)
  =>  (tag: ClassTag[traversable.Operand])
  =>  self is Convertible in IArray to IArray[traversable.Operand] =
    self => IArray.from(traversable.traverse(self))(using tag)

  given text: [self] => (traversable: self is Traversable by Char)
  =>  self is Convertible in Text to Text =
    self => Text(traversable.traverse(self).mkString)

trait Convertible extends Typeclass.Pure, Resultant:
  type Form <: AnyKind
  def convert(self: Self): Result
