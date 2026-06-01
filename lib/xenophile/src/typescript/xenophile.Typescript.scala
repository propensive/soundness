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
import distillate.*
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

  // A TypeScript `T[]` (i.e. `Array<T>`) maps to a Scala `List`, with each element converted by
  // the element type's own `Interoperable`.
  given list: [element, topic]
  =>  ( interoperable: element is Interoperable in Typescript of topic by Json )
  =>  ( List[element] is Interoperable in Typescript of ("Array" over topic) by Json ) =
    Interoperable[List[element], Typescript, ("Array" over topic), Json]
      ( _.map(interoperable.operand(_)).json,
        _.as[List[Json]].map(interoperable.value(_)) )

  // TypeScript `undefined` (produced by reading `T?` as `T | undefined`) maps to the absent
  // `Optional` value; it also backs the `Unset.type` alternative when a union is decoded.
  given undefined: (Unset.type is Interoperable in Typescript of "undefined" by Json) =
    Interoperable[Unset.type, Typescript, "undefined", Json](_ => Json(Unset), _ => Unset)

  // A TypeScript `T?` (read as `T | undefined`) maps to a Scala `Optional`. Mirroring jacinta's own
  // optional codecs, the `Mandatable` constraint identifies the mandatory type `inner` and ensures
  // this instance applies only to genuine optionals — so it never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( interoperable: inner is Interoperable in Typescript of topic by Json )
  =>  ( value is Interoperable in Typescript of (topic | "undefined") by Json ) =
    Interoperable[value, Typescript, (topic | "undefined"), Json]
      ( _.let(_.asInstanceOf[inner]).let(interoperable.operand(_)).or(Json(Unset)),
        _.as[Optional[Json]].let(interoperable.value(_)) )

  // A TypeScript `Map<K, V>` maps to a Scala `Map`. Values are converted by their `Interoperable`;
  // JSON object keys are textual, so the key type also needs `Text` codecs.
  given map: [key, value, keyTopic, valueTopic]
  =>  ( keyType:   key is Interoperable in Typescript of keyTopic by Json,
        valueType: value is Interoperable in Typescript of valueTopic by Json,
        keyEncode: key is Encodable in Text,
        keyDecode: key is Decodable in Text )
  =>  ( Map[key, value] is Interoperable in Typescript
          of ("Map" over (keyTopic, valueTopic)) by Json ) =

    Interoperable[Map[key, value], Typescript, ("Map" over (keyTopic, valueTopic)), Json]
      ( _.map { (k, v) => (k.encode, valueType.operand(v)) }.json,
        _.as[Map[Text, Json]].map { (k, v) => (k.decode[key], valueType.value(v)) } )

  // A backend that evaluates a `ForeignExpr` against an in-memory JSON document: references and
  // selections navigate the document; literals yield their operand; function application is
  // unsupported (a static document has no callable members).
  def evaluator(document: Json): Evaluator in Typescript by Json =
    new Evaluator:
      type Form = Typescript
      type Operand = Json

      def evaluate(expr: ForeignExpr): Json = expr match
        case ForeignExpr.Literal(value)         => value.asInstanceOf[Json]
        case ForeignExpr.Reference(name)        => document(name)
        case ForeignExpr.Select(target, member) => evaluate(target)(member)

        case ForeignExpr.Apply(_, _) =>
          throw RuntimeException("xenophile: a JSON document evaluator cannot apply functions")

trait Typescript extends Ecosystem:
  type Operand = Json
  type Grammar = TypescriptDialect.type
