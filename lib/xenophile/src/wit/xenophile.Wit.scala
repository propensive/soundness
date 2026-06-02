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
package xenophile

import anticipation.*
import contingency.*, strategies.throwUnsafely
import jacinta.*
import prepositional.*
import vacuous.*

// The WIT (WebAssembly Interface Types) ecosystem. Foreign values are represented as `Json` (the
// component model's textual value representation), grammars are read from `.wit` files by
// `WitDialect`, and primitives map to the obvious Scala types.
object Wit:
  // WIT integers all canonicalise (in `WitDialect`) to `s32` (8/16/32-bit) or `s64` (64-bit), and
  // `char` to `string`, so each Scala type maps to exactly one WIT primitive.
  given s32: (Int is Interoperable in Wit of "s32" by Json) =
    Interoperable[Int, Wit, "s32", Json](_.json, _.as[Int])

  given s64: (Long is Interoperable in Wit of "s64" by Json) =
    Interoperable[Long, Wit, "s64", Json](_.json, _.as[Long])

  given f32: (Float is Interoperable in Wit of "f32" by Json) =
    Interoperable[Float, Wit, "f32", Json](_.json, _.as[Float])

  given f64: (Double is Interoperable in Wit of "f64" by Json) =
    Interoperable[Double, Wit, "f64", Json](_.json, _.as[Double])

  given boolean: (Boolean is Interoperable in Wit of "bool" by Json) =
    Interoperable[Boolean, Wit, "bool", Json](_.json, _.as[Boolean])

  given string: (Text is Interoperable in Wit of "string" by Json) =
    Interoperable[Text, Wit, "string", Json](_.json, _.as[Text])

  // A WIT `list<T>` maps to a Scala `List`, each element converted by its own `Interoperable`.
  given list: [element, topic]
  =>  ( interoperable: element is Interoperable in Wit of topic by Json )
  =>  ( List[element] is Interoperable in Wit of ("list" over topic) by Json ) =
    Interoperable[List[element], Wit, ("list" over topic), Json]
      ( _.map(interoperable.operand(_)).json,
        _.as[List[Json]].map(interoperable.value(_)) )

  // WIT `none` (produced by reading `option<T>` as `T | none`) maps to the absent `Optional` value.
  given none: (Unset.type is Interoperable in Wit of "none" by Json) =
    Interoperable[Unset.type, Wit, "none", Json](_ => Json(Unset), _ => Unset)

  // A WIT `option<T>` (read as `T | none`) maps to a Scala `Optional`. As in jacinta's own optional
  // codecs, the `Mandatable` constraint identifies the mandatory type `inner` and ensures this
  // instance applies only to genuine optionals, so it never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( interoperable: inner is Interoperable in Wit of topic by Json )
  =>  ( value is Interoperable in Wit of (topic | "none") by Json ) =
    Interoperable[value, Wit, (topic | "none"), Json]
      ( _.let(_.asInstanceOf[inner]).let(interoperable.operand(_)).or(Json(Unset)),
        _.as[Optional[Json]].let(interoperable.value(_)) )

  // A backend that evaluates a `Foreign.Expression` against an in-memory JSON document: references
  // and selections navigate the document; literals yield their operand; function application is
  // unsupported (a static document has no callable functions).
  def apply(document: Json): Evaluator in Wit by Json =
    new Evaluator:
      type Form = Wit
      type Operand = Json

      def evaluate(expr: Foreign.Expression): Json = expr match
        case Foreign.Expression.Literal(value)            => value.asInstanceOf[Json]
        case Foreign.Expression.Reference(name)           => document(name)
        case Foreign.Expression.Select(target, member, _) => evaluate(target)(member)

        case Foreign.Expression.Apply(_, _) =>
          throw RuntimeException("xenophile: a WIT document evaluator cannot call functions")

trait Wit extends Ecosystem:
  type Operand = Json
  type Grammar = WitDialect.type
