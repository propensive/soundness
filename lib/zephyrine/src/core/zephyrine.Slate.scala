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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package zephyrine

import scala.caps

import denominative.*
import prepositional.*
import vacuous.*

// The writable counterpart of `Region`, for duct output storage: `update` in place of
// `apply`, with the same branding discipline. See `zephyrine.Region.scala` for the design.
// The accessors live in the companion (found through implicit scope) rather than at the top
// level: top-level extensions in separate files of one package cannot overload `Region`'s.
opaque type Slate[medium] = AnyRef

object Slate:
  // As `Region.over`, but lending write access: the storage must be exclusive.
  inline def over[medium, result](using addressable: medium is Addressable)
    ( storage: addressable.Storage^{caps.any}, offset: Int, limit: Int )
    ( inline lambda: (slate: Slate[medium]) => (Interval in slate.type) => result )
  :   result =

    // The cast erases the exclusive capture for the read-only size query: passing an
    // exclusive reference into a `{caps.any.rd}` parameter is rejected when `Storage` is
    // abstract, and nothing of the reference is retained.
    val size = addressable.storageSize(storage.asInstanceOf[addressable.Storage])
    val start = offset.max(0).min(size)
    val end = limit.max(start).min(size)
    val slate: Slate[medium] = storage.asInstanceOf[AnyRef]
    lambda(slate)(Interval.zerary(start, end).asInstanceOf[Interval in slate.type])

  extension [medium](slate: Slate[medium])(using addressable: medium is Addressable)
    inline def update(index: Ordinal in slate.type, operand: addressable.Operand): Unit =
      val ordinal: Ordinal = index
      addressable.storageUpdate(slate.asInstanceOf[addressable.Storage^], ordinal.n0, operand)

    inline def visit(range: Interval in slate.type)
      ( inline lambda: (Ordinal in slate.type) => Unit )
    :   Unit =

      val interval: Interval = range
      var index: Int = interval.start.n0
      val end: Int = interval.limit.n0

      while index < end do
        lambda(Ordinal.zerary(index).asInstanceOf[Ordinal in slate.type])
        index += 1

    inline def raw(using Unsafe): addressable.Storage^ =
      slate.asInstanceOf[addressable.Storage^]
