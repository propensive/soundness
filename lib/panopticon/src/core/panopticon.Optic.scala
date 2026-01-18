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
import denominative.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

import scala.quoted.*
import scala.compiletime.*

import language.dynamics
import scala.annotation.internal.preview


object Optic:
  transparent inline given deref: [name <: Label, product <: Product] => name is Lens from product =
    ${Panopticon.lens[name, product]}

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

  given prim: [element]
               => Prim.type is Optic from List[element] to List[element] by element onto element =
    Optic[Prim.type, List[element], List[element], element, element]: (origin, lambda) =>
      origin match
        case head :: tail => lambda(head) :: tail
        case Nil          => Nil

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


  def update[traversal, result](traversal: traversal, value: result)
       (using optic: traversal.type is Optic from Operand to Target, equality: result <:< optic.Target)
  : Origin => Result =

      Composable.optics.composition(this, optic).modify(_)(_ => equality(value))


  def applyDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)[target]
        (traversal: Any)
        (using optic: traversal.type is Optic from lens.Operand to lens.Target onto target)
  : Optic from Origin to Result by optic.Operand onto target =

      Composable.optics.composition(Composable.optics.composition(this, lens), optic)
