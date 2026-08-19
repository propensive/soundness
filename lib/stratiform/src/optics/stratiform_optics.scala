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
package stratiform

import anticipation.*
import contingency.*
import denominative.*
import panopticon.*
import prepositional.*
import vacuous.*

// The panopticon lens and optic instances for `Tel`. These were members of `trait Tel2`, and so
// inherited into `Tel`'s implicit scope; they are the only reason `stratiform.core` needed
// panopticon, so they live here as toplevel givens instead. As with jacinta's equivalents, a
// call site that fails to import them cannot silently degrade: panopticon's generic `deref`
// lens applies only to `Product` types, and `Tel` is a plain class.

// Field-keyed lens: a name `<: Label` resolves to a Lens from `Tel`
// onto `Tel`. The getter delegates to `selectDynamic`; the setter
// routes through `Tel.modify`, which replaces an existing child
// compound with the same kebab-case keyword in place or appends a
// new one. Mirrors jacinta's lens given.
package optics:
  given telLens: [name <: Label: ValueOf] => (erased dynamicTelEnabler: DynamicTelEnabler) => Tactic[Tel.Error]
  =>  name is Lens from Tel onto Tel =
    Lens(_.selectField(valueOf[name]), _.modify(valueOf[name], _))

  // Positional optics over a node's child compounds (TEL has no positional arrays,
  // but a compound's children are ordered — this mirrors the read-side
  // `applyDynamic(field)(index)`). `Ordinal` addresses the n-th child; `Each` every
  // child. The transform's result keeps the original child's keyword, so a positional
  // update preserves the field identity while replacing its value/children.
  // (`rewrap`/`rebuild` are package-level pure helpers — see `stratiform_core.scala`.)

  given telOrdinalOptical: [element] => Ordinal is Optical from Tel onto Tel = ordinal =>
    Optic: (origin, lambda) =>
      if ordinal.n0 < 0 || ordinal.n0 >= origin.childCompounds.length then origin
      else rebuild
        ( origin,
          Tel.withChildCompound
           ( origin.subtree.children, ordinal.n0, c => rewrap(c, lambda(Tel.make(c))) ) )

  given telEachOptical: Each.type is Optical from Tel onto Tel = _ =>
    Optic: (origin, lambda) =>
      rebuild
        ( origin,
          Tel.mapChildCompounds(origin.subtree.children, c => rewrap(c, lambda(Tel.make(c)))) )

