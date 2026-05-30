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

object Typescript:
  given text: (Text is Interoperable in Typescript of "string" by Json) =
    Interoperable[Text, Typescript, "string", Json](_.json, _.as[Text])

  given int: (Int is Interoperable in Typescript of "number" by Json) =
    Interoperable[Int, Typescript, "number", Json](_.json, _.as[Int])

  given boolean: (Boolean is Interoperable in Typescript of "boolean" by Json) =
    Interoperable[Boolean, Typescript, "boolean", Json](_.json, _.as[Boolean])

  // A TypeScript `string[]` array maps to a Scala `List[Text]`, decoded with jacinta's own
  // collection support.
  given strings: (List[Text] is Interoperable in Typescript of "string[]" by Json) =
    Interoperable[List[Text], Typescript, "string[]", Json](_.json, _.as[List[Text]])

  // A backend that evaluates a `ForeignExpr` against an in-memory JSON document: references and
  // selections navigate the document; literals yield their operand; function application is
  // unsupported (a static document has no callable members).
  def evaluator(document: Json): Evaluator in Typescript by Json =
    new Evaluator:
      type Form = Typescript
      type Operand = Json

      def absent(operand: Json): Boolean = operand.as[Optional[Json]] == Unset

      def evaluate(expr: ForeignExpr): Json = expr match
        case ForeignExpr.Literal(value)         => value.asInstanceOf[Json]
        case ForeignExpr.Reference(name)        => document(name)
        case ForeignExpr.Select(target, member) => evaluate(target)(member)

        case ForeignExpr.Apply(_, _) =>
          throw RuntimeException("xenophile: a JSON document evaluator cannot apply functions")

trait Typescript extends Ecosystem:
  type Operand = Json
