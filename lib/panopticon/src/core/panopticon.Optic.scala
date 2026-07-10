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
package panopticon

import language.dynamics

import scala.quoted.*

import beneficence.Findable
import denominative.*
import prepositional.*

object Optic:
  transparent inline given deref: [name <: Label, product <: Product] => name is Lens from product =
    ${panopticon.internal.lens[name, product]}

  def identity[value]: Optic from value onto value = new Optic:
    type Origin = value
    type Target = value

    def modify(origin: Origin)(lambda: Target => Target): Origin = lambda(origin)


  // The optic stores `lambda`, so the constructed instance captures whatever `lambda` captures
  // (`^{lambda}`, exactly like `LzyList.map(f): LzyList[B]^{xs, f}`). A pure transform yields a pure
  // optic; a transform that closes over a `Tactic` yields a capturing optic, so fallibility flows
  // honestly into the optic rather than being laundered away.
  def apply[self, origin, target](lambda: (origin, target => target) => origin)
  :   (self is Optic from origin onto target)^{lambda} =

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

// `Optic` extends `Findable` (not the pure `Typeclass`): an optic that wraps a fallible transform
// captures that transform's capabilities, so it must be capture-tracked. A pure optic still captures
// nothing, so existing pure usage is unaffected.
trait Optic extends Findable, Dynamic:
  type Self
  type Origin
  type Target

  def modify(origin: Origin)(lambda: Target => Target): Origin


  // Compose `this` with a following optic. Since `Optic` is an extensible trait, `this` has the
  // universal capture, which cannot flow into a `^` *parameter* of `Composable.composition`; so the
  // composition is built here, capturing `this` directly via the closure (the `def fn2: B^ = this`
  // trait idiom). The result is a capturing `Optic^`; fallibility is still tracked precisely because
  // the fallible optic given itself demands a `Tactic` to be summoned where it is used.
  private def andThen[next](following: (Optic from Target onto next)^)
  :   (Optic from Origin onto next)^ =

    Optic[Any, Origin, next]: (origin, lambda) =>
      this.modify(origin)(following.modify(_)(lambda))


  def selectDynamic(name: Label)(using lens: name.type is Optic from Target)
  :   (Optic from Origin onto lens.Target)^ =

    andThen(lens)


  def updateDynamic(name: Label)(using lens: name.type is Optic from Target)
    [ source ]
    ( value: (lens.Target aka "prior") ?=> source )
    ( using coercible: source is Coercible to lens.Target )
  :   Origin => Origin =

    andThen(lens).modify(_): prior =>
      coercible.coerce(value(using prior.aka["prior"]))


  def update[source, target](traversal: Any, value: source)
    ( using optical:  (? >: traversal.type) is Optical from Target onto target,
            coercible: source is Coercible to target )
  :   Origin => Origin =

    andThen(optical.optic(traversal)).modify(_): _ =>
      coercible.coerce(value)


  def applyDynamic(name: Label)[operand](using lens: name.type is Optic from Target onto operand)
    [ target, traversal ]
    ( traversal: traversal )
    ( using optical: (? >: traversal.type) is Optical from operand onto target )
  :   (Optic from Origin onto target)^ =

    andThen(lens).andThen(optical.optic(traversal))


  def apply[target, optic](traversal: optic)
    ( using optical: (? >: traversal.type) is Optical from Target onto target )
  :   (Optic from Origin onto target)^ =

    andThen(optical.optic(traversal))
