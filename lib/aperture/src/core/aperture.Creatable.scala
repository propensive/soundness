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
package aperture

import prepositional.*

// An entity which can be created: the counterpart of `Openable`, for bringing artifacts into
// existence rather than accessing existing ones. An instance is written
// `target is Creatable in Form by Flag to Handle`, and serves two notions of creation with
// one verb:
//
//  - *instantiation*: `path.create[Directory]()` makes an empty artifact exist, with no
//    scope, returning the target;
//  - *scoped authoring*: `path.create[Zip](): zip ?=> ...` creates the artifact, populates
//    it through a handle granted `Grants` (write access from birth: a newborn artifact is
//    unconditionally its creator's), and commits the result when the scope closes. An
//    exception escaping the scope means nothing is left behind: instances guarantee this by
//    staging to a temporary sibling and moving atomically, or by wiping what they created.
//
// There is no `Mode` parameter: what a creation scope may do is not a caller's choice, and
// the persisted permissions of the created entry are flag territory. Creating an entity that
// might already exist is governed by per-form flags (replacement is never the default), and
// is distinct from *opening* a possibly-absent entity for writing (`OpenFlag.Create`), which
// accesses whatever is there.
trait Creatable extends Typeclass, Formal, Operable, Resultant:
  type Grants <: Grant

  // Creates the artifact, empty. The default serves builder forms, whose discarded handle
  // costs nothing; forms holding OS resources override it to create directly — notably a
  // FIFO, which must never be opened just to be created (opening one for writing blocks
  // until a reader appears).
  def make(value: Self, flags: List[Operand]): Unit =
    create(value, flags) { () }

  def create[result]
    ( value: Self, flags: List[Operand] )
    ( block: ((Result & Granting[Grants])^) ?=> result )
  :   result
