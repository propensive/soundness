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
package panopticon

import denominative.*
import prepositional.*
import proscenium.*

import scala.quoted.*

import language.dynamics

object Optic:
  transparent inline given deref: [name <: Label, product <: Product] => name is Lens from product =
    ${Panopticon.lens[name, product]}

  def identity[value]: Optic from value onto value = new Optic:
    type Origin = value
    type Target = value

    def modify(origin: Origin)(lambda: Target => Target): Origin = lambda(origin)

  def apply[self, origin, target](lambda: (origin, target => target) => origin)
  :   self is Optic from origin onto target =

      new Optic:
        type Self = self
        type Origin = origin
        type Target = target

        def modify(origin: Origin)(lambda2: Target => Target): Origin = lambda(origin, lambda2)

  given prim: [element]
  =>  Prim.type is Optic from List[element] onto element =

      Optic[Prim.type, List[element], element]: (origin, lambda) =>
        origin match
          case head :: tail => lambda(head) :: tail
          case Nil          => Nil

trait Optic extends Typeclass, Dynamic:
  type Origin
  type Target

  def modify(origin: Origin)(lambda: Target => Target): Origin

  def selectDynamic(name: Label)(using lens: name.type is Optic from Target)
  :   Optic from Origin onto lens.Target =

      Composable.optics.composition(this, lens)


  def updateDynamic(name: Label)(using lens: name.type is Optic from Target)
    ( value: (prior: lens.Target) ?=> lens.Target )
  :   Origin => Origin =

      Composable.optics.composition(this, lens).modify(_)(value(using _))


  def update[target](traversal: Any, value: target)
    ( using optical: (? >: traversal.type) is Optical from Target onto (? >: target) )
  :   Origin => Origin =

      Composable.optics.composition
        ( this, optical.optic(traversal)).modify(_)(_ => value )


  def applyDynamic(name: Label)[operand](using lens: name.type is Optic from Target onto operand)
    [ target, traversal ]
    ( traversal: traversal )
    ( using optical: (? >: traversal.type) is Optical from operand onto target )
  :   Optic from Origin onto target =

      Composable.optics.composition
        ( Composable.optics.composition(this, lens), optical.optic(traversal) )

  def apply[target, optic](traversal: optic)
    ( using optical: (? >: traversal.type) is Optical from Target onto target )
  :   Optic from Origin onto target =

      Composable.optics.composition(this, optical.optic(traversal))
