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
import rudiments.*
import vacuous.*

import RatelLayout.{Entry, Kind, Layout, Payload}

// What the members of a record read as, through a `Ratel.Provider`: the one description from
// which the provider's macro makes the type of a record, and against which `Ratel.Row` reads a
// member when it is selected, so that the two cannot disagree.
private[stratiform] object RatelShape:
  // The Scala type a scalar's values have: a number, for an encoding whose every value one
  // holds exactly, and otherwise the text its codec gives.
  enum Primitive:
    case Whole, Large, Real, Textual

  enum Arity:
    case One, Optional, Many

  enum Shape:
    // A flag which is not repeatable is whether it occurs, and one which is, how often.
    case Flag
    case Count
    case Value(primitive: Primitive, arity: Arity)

    // A struct, and a repeatable struct. A struct which contains itself has no fields here,
    // where it does: a type cannot go on for ever, so such a record is read as a row.
    case Nested(fields: Optional[List[(Text, Shape)]], optional: Boolean)
    case Table(fields: Optional[List[(Text, Shape)]])

  def primitive(scalar: Tels.Scalar): Primitive = scalar.encoding.lay(Primitive.Textual):
    case t"u8" | t"u16" | t"i8" | t"i16" | t"i32" => Primitive.Whole
    case t"u32" | t"i64"                          => Primitive.Large
    case t"f64"                                   => Primitive.Real
    case _                                        => Primitive.Textual

  // A value of a scalar, from the text its codec gives.
  def value(scalar: Tels.Scalar, text: Text): Any = primitive(scalar) match
    case Primitive.Whole   => Integer.parseInt(text.s)
    case Primitive.Large   => java.lang.Long.parseLong(text.s)
    case Primitive.Real    => java.lang.Double.parseDouble(text.s)
    case Primitive.Textual => text

  // The members of the document's root.
  def apply(layout: Layout): List[(Text, Shape)] =
    layout.table(0).lay(Nil) { table => fields(layout, table.entries, List(0)) }

  private def struct(layout: Layout, table: Int, visited: List[Int])
  :   Optional[List[(Text, Shape)]] =

    if visited.has(table) then Unset
    else layout.table(table).let { plan => fields(layout, plan.entries, table :: visited) }

  private def fields(layout: Layout, entries: List[Entry], visited: List[Int])
  :   List[(Text, Shape)] =

    entries.bind:
      case Entry.Flattened(_, keyword, _, inner) =>
        List(keyword -> Shape.Nested(fields(layout, inner, visited), false))

      case Entry.Stored(slot) =>
        val arity = if slot.required then Arity.One else Arity.Optional

        slot.kind match
          case Kind.Always | Kind.Flag => List(slot.keyword -> Shape.Flag)
          case Kind.FlagCount          => List(slot.keyword -> Shape.Count)
          case Kind.Inline(scalar, _)  => List(slot.keyword -> Shape.Value(primitive(scalar), arity))
          case Kind.Ref(scalar, _)     => List(slot.keyword -> Shape.Value(primitive(scalar), arity))

          case Kind.Packed(scalar, _) =>
            List(slot.keyword -> Shape.Value(primitive(scalar), Arity.Many))

          case Kind.Refs(scalar, _) =>
            List(slot.keyword -> Shape.Value(primitive(scalar), Arity.Many))

          case Kind.Record(table, _) =>
            List(slot.keyword -> Shape.Nested(struct(layout, table, visited), !slot.required))

          case Kind.Range(table, _) =>
            List(slot.keyword -> Shape.Table(struct(layout, table, visited)))

          // Whichever variant of a select is chosen, the others are absent.
          case Kind.Select(variants) => variants.map: variant =>
            variant.keyword -> variant.payload.match
              case Payload.Flag                => Shape.Flag
              case Payload.Value(scalar, _)    => Shape.Value(primitive(scalar), Arity.Optional)
              case Payload.Record(table, _)    => Shape.Nested(struct(layout, table, visited), true)

          // The variants of a repeatable select are read from the row, with `materialise`.
          case Kind.SelectList(_) => Nil
