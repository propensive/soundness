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

import scala.collection.Iterator as ScalaIterator
import scala.collection.IterableOnce as ScalaIterableOnce

import vacuous.*
import prepositional.*

object Traversable:
  given list: [element] => List[element] is Traversable:
    type Operand = element
    def iterator(self: List[element]): ScalaIterator[element] = self.scala.iterator

  given set: [element] => Set[element] is Traversable:
    type Operand = element
    def iterator(self: Set[element]): ScalaIterator[element] = self.scala.iterator

  given map: [key, value] => Map[key, value] is Traversable:
    type Operand = (key, value)
    def iterator(self: Map[key, value]): ScalaIterator[(key, value)] = self.scala.iterator

  given series: [element] => Series[element] is Traversable:
    type Operand = element
    def iterator(self: Series[element]): ScalaIterator[element] = self.scala.iterator

  given iarray: [element] => IArray[element] is Traversable:
    type Operand = element
    def iterator(self: IArray[element]): ScalaIterator[element] = self.iterator

  given iterableOnce: [element] => IterableOnce[element] is Traversable:
    type Operand = element
    def iterator(self: IterableOnce[element]): ScalaIterator[element] = self.iterator

  given scalaCollection: [collection[item] <: ScalaIterableOnce[item], element]
      => collection[element] is Traversable:
    type Operand = element
    def iterator(self: collection[element]): ScalaIterator[element] = self.iterator

trait Traversable extends Typeclass, Operable:
  def iterator(self: Self): ScalaIterator[Operand]

extension [traversable: Traversable](value: traversable)
  inline def head: traversable.Operand = traversable.iterator(value).next()
  inline def headOption: Option[traversable.Operand] = traversable.iterator(value).nextOption()

  inline def last: traversable.Operand =
    traversable.iterator(value).reduceLeft((_, next) => next)

  inline def lastOption: Option[traversable.Operand] =
    traversable.iterator(value).reduceLeftOption((_, next) => next)


  inline def foldLeft[total](initial: total)(lambda: (total, traversable.Operand) => total): total =
    traversable.iterator(value).foldLeft(initial)(lambda)

  inline def foldRight[total](initial: total)(lambda: (traversable.Operand, total) => total): total =
    traversable.iterator(value).foldRight(initial)(lambda)

  inline def fold(initial: traversable.Operand)
      (lambda: (traversable.Operand, traversable.Operand) => traversable.Operand)
  :   traversable.Operand =
    traversable.iterator(value).fold(initial)(lambda)

  inline def reduce(lambda: (traversable.Operand, traversable.Operand) => traversable.Operand)
  :   traversable.Operand =
    traversable.iterator(value).reduce(lambda)

  inline def reduceLeft(lambda: (traversable.Operand, traversable.Operand) => traversable.Operand)
  :   traversable.Operand =
    traversable.iterator(value).reduceLeft(lambda)

  inline def exists(predicate: traversable.Operand => Boolean): Boolean =
    traversable.iterator(value).exists(predicate)

  inline def find(predicate: traversable.Operand => Boolean): Option[traversable.Operand] =
    traversable.iterator(value).find(predicate)

  inline def count(predicate: traversable.Operand => Boolean): Int =
    traversable.iterator(value).count(predicate)

  inline def sum(using Numeric[traversable.Operand]): traversable.Operand =
    traversable.iterator(value).sum

  inline def min(using Ordering[traversable.Operand]): traversable.Operand =
    traversable.iterator(value).min

  inline def max(using Ordering[traversable.Operand]): traversable.Operand =
    traversable.iterator(value).max

  inline def minOption(using Ordering[traversable.Operand]): Option[traversable.Operand] =
    traversable.iterator(value).minOption

  inline def maxOption(using Ordering[traversable.Operand]): Option[traversable.Operand] =
    traversable.iterator(value).maxOption

  inline def minBy[key](lambda: traversable.Operand => key)(using Ordering[key]): traversable.Operand =
    traversable.iterator(value).minBy(lambda)

  inline def maxBy[key](lambda: traversable.Operand => key)(using Ordering[key]): traversable.Operand =
    traversable.iterator(value).maxBy(lambda)

  inline def collectFirst[operand2](lambda: PartialFunction[traversable.Operand, operand2])
  :   Option[operand2] =
    traversable.iterator(value).collectFirst(lambda)
