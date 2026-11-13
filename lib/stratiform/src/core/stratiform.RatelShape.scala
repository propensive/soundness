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
import gossamer.*
import polyvinyl.Specification.{Member, Multiplicity}
import rudiments.*
import vacuous.*

import RatelLayout.{Entry, Kind, Layout, Payload}

// What the members of a record read as, through a `Ratel.Provider`: the layout of a schema as
// the members of a polyvinyl specification.
private[stratiform] object RatelShape:
  // The label a scalar's `Intensional` instance is found by: its encoding, if it has one, since
  // that decides what its values are, and its first validator if not.
  def label(scalar: Tels.Scalar): Text =
    scalar.encoding.or(scalar.validators.head.or(t"string"))

  // The members of the document's root.
  def apply(layout: Layout): List[(Text, Member)] =
    layout.table(0).lay(Nil) { table => fields(layout, table.entries, List(0)) }

  // The members of a struct, unless it is one which is already being described: a struct which
  // contains itself is read as its cell, since a type cannot go on for ever.
  private def struct(layout: Layout, table: Int, visited: List[Int], multiplicity: Multiplicity)
  :   Member =

    if visited.has(table) then Member.Value(t"cell", Nil, multiplicity) else
      layout.table(table).lay(Member.Value(t"cell", Nil, multiplicity)): plan =>
        Member.Record(fields(layout, plan.entries, table :: visited), multiplicity)

  private def fields(layout: Layout, entries: List[Entry], visited: List[Int])
  :   List[(Text, Member)] =

    entries.bind:
      case Entry.Flattened(_, keyword, _, inner) =>
        List(keyword -> Member.Record(fields(layout, inner, visited)))

      case Entry.Stored(slot) =>
        val arity = if slot.required then Multiplicity.One else Multiplicity.Optional
        def value(scalar: Tels.Scalar, multiplicity: Multiplicity): Member =
          Member.Value(label(scalar), Nil, multiplicity)

        slot.kind match
          // A flag which is not repeatable is whether it occurs, and one which is, how often.
          case Kind.Always | Kind.Flag => List(slot.keyword -> Member.Value(t"flag"))
          case Kind.FlagCount          => List(slot.keyword -> Member.Value(t"count"))
          case Kind.Inline(scalar, _)  => List(slot.keyword -> value(scalar, arity))
          case Kind.Ref(scalar, _)     => List(slot.keyword -> value(scalar, arity))
          case Kind.Packed(scalar, _)  => List(slot.keyword -> value(scalar, Multiplicity.Many))
          case Kind.Refs(scalar, _)    => List(slot.keyword -> value(scalar, Multiplicity.Many))

          case Kind.Record(table, _) =>
            List(slot.keyword -> struct(layout, table, visited, arity))

          case Kind.Range(table, _) =>
            List(slot.keyword -> struct(layout, table, visited, Multiplicity.Series))

          // Whichever variant of a select is chosen, the others are absent.
          case Kind.Select(variants) => variants.map: variant =>
            variant.keyword -> variant.payload.match
              case Payload.Flag             => Member.Value(t"flag")
              case Payload.Value(scalar, _) => value(scalar, Multiplicity.Optional)
              case Payload.Record(table, _) => struct(layout, table, visited, Multiplicity.Optional)

          // The variants of a repeatable select are read from the row, with `materialise`.
          case Kind.SelectList(_) => Nil
