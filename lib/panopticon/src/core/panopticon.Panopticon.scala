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

  def apply[self, origin, result, operand, target](lambda: (origin, operand => target) => result)
  : self is Optic from origin to result by operand onto target =

      new Optic:
        type Self = self
        type Origin = origin
        type Target = target
        type Operand = operand
        type Result = result

        def modify(origin: Origin)(lambda2: Operand => Target): Result = lambda(origin, lambda2)

trait Optic extends Typeclass, Dynamic:
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

      Composable.optics.composition(this, lens).modify(_)(value(using _))


  def update
       [result, operand, traversal: Optic from Operand to Target by operand onto result as optic]
       (traversal: traversal, value: (prior: optic.Operand) ?=> result)
  : Origin => Result =

      Composable.optics.composition(this, optic).modify(_)(value(using _))


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
      Optic[Any, origin, result, operand2, target2]: (origin, lambda) =>
        left.modify(origin)(right.modify(_)(lambda))

  given lenses: [origin, target, target2]
        => (Lens from origin onto target) is Composable by (Lens from target onto target2) to
               (Lens from origin onto target2) =
    (left, right) =>
      Lens[Any, origin, target2]
       ({ origin => right(left(origin)) },
        { (origin, value) => left(origin) = right(left(origin)) = value })

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
    Optic[Each.type, List[element], List[element], element, element](_.map(_))

object Head:
  given optic: [element]
               => Head.type is Optic from List[element] to List[element] by element onto element =
    Optic[Head.type, List[element], List[element], element, element]: (origin, lambda) =>
      origin match
        case head :: tail => lambda(head) :: tail
        case Nil          => Nil

extension [value](value: value)
  def lens(lambdas: (Optic from value to value by value onto value => value => value)*): value =
    lambdas.foldLeft(value): (value, lambda) =>
      lambda(Optic.identity)(value)
