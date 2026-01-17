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
┃    Soundness, version 0.51.0.                                                                    ┃
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
package panopticon

import anticipation.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

import scala.quoted.*
import scala.compiletime.*

import language.dynamics
import scala.annotation.internal.preview

object Panopticon:
  private given realm: Realm = realm"panopticon"

object Optic:
  def identity[value]: Optic from value to value by value onto value =
    new Optic:
      type Origin = value
      type Result = value
      type Operand = value
      type Target = value

      def apply(origin: Origin): Operand = origin
      def update(origin: Origin, value: Target): Result = value

      def modify(origin: Origin)(lambda: Operand => Target): Result = lambda(origin)



trait Optic extends Typeclass, Dynamic:
  lens0 =>
    type Origin
    type Result
    type Operand
    type Target

    def modify(origin: Origin)(lambda: Operand => Target): Result

    def selectDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
    : Optic from Origin to Result by lens.Operand onto lens.Target =

        Composable.optics.composition(this, lens)


    def updateDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
         (value: (prior: lens.Operand) ?=> lens.Target)
    : Origin => Result =
        origin =>
          Composable.optics.composition(this, lens).modify(origin)(value(using _))

    def update[result, operand, traversal: Optic from Operand to Target by operand onto result as optic]
         (traversal: traversal, value: (prior: operand) ?=> result)
    : Origin => Result =
        origin =>
          Composable.optics.composition(this, optic).modify(origin)(value(using _))

    def applyDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
         [target, traversal: Optic from lens.Operand to lens.Target onto target as optic]
         (traversal: traversal)
    : Optic from Origin to Result by optic.Operand onto target =
        Composable.optics.composition(Composable.optics.composition(this, lens), optic)

object Composable:

  given optics: [origin, result, operand, target, operand2, target2]
        => (Optic from origin to result by operand onto target) is Composable by
            (Optic from operand to target by operand2 onto target2) to
            (Optic from origin to result by operand2 onto target2) =
    (left, right) =>
      new Optic:
        type Origin = origin
        type Result = result
        type Operand = operand2
        type Target = target2

        def modify(origin: origin)(lambda: Operand => target2): result =
          left.modify(origin)(right.modify(_)(lambda))

  given lenses: [origin, target, target2]
        => (Lens from origin onto target) is Composable by
            (Lens from target onto target2) to
            (Lens from origin onto target2) =
    (left, right) =>
      new Lens:
        type Origin = origin
        type Target = target2

        def apply(origin: Origin): Operand = right(left(origin))

        def update(origin: Origin, value: Target): Result =
          left(origin) = right(left(origin)) = value

object Lens:

  def apply[self, origin, target](get: origin => target, set: (origin, target) => origin)
  : self is Lens from origin onto target =
    new Lens:
      type Self = self
      type Origin = origin
      type Target = target

      def apply(origin: Origin): Operand = get(origin)
      def update(origin: Origin, value: Target): Result = set(origin, value)

trait Lens extends Optic:
  type Result = Origin
  type Operand = Target

  def apply(origin: Origin): Operand
  def update(origin: Origin, value: Operand): Origin

  def modify(origin: Origin)(lambda: Operand => Target): Origin =
    update(origin, lambda(apply(origin)))


trait Composable:
  type Self
  type Operand
  type Result

  def composition(left: Self, right: Operand): Result

extension [value](left: value)
  def compose[operand, result](right: operand)
       (using composable: value is Composable by operand to result)
  : result =
      composable.composition(left, right)

object Each:
  given optic: [element]
               => Each.type is Optic from List[element] to List[element] by element onto element =
    new Optic:
      type Self = Each.type
      type Origin = List[element]
      type Target = element
      type Result = List[element]
      type Operand = element

      def modify(list: List[element])(lambda: element => element): List[element] = list.map(lambda)

object Head:
  given optic: [element]
               => Head.type is Optic from List[element] to List[element] by element onto element =
    new Optic:
      type Self = Head.type
      type Origin = List[element]
      type Operand = element
      type Result = List[element]
      type Target = element

      def modify(list: List[element])(lambda: element => element): List[element] = list match
        case head :: tail => lambda(head) :: tail
        case Nil          => Nil

extension [value](value: value)
  def lens(lambdas: (Optic from value to value by value onto value => value => value)*): value =
    lambdas.foldLeft(value): (value, lambda) =>
      lambda(Optic.identity)(value)
