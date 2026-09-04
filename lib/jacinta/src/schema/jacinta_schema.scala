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
package jacinta


import scala.annotation.*

import anticipation.*
import murmuration.{exists, foreach}
import contingency.*
import prepositional.*
import telekinesis.*
import urticose.*
import vacuous.*
import wisteria.*
import zephyrine.*

import httpBackends.javaNetHttp
import Json.Error.Reason

package jsonPointerRegistries:
  given standaloneRegistry: JsonPointer.Registry:
    protected def lookup(url: HttpUrl): Optional[Json] = Unset

  // The registry retains the HTTP client it fetches through, so the instance is a
  // capability — a given constructed from capabilities produces a capability (Jon,
  // 2026-07-06; see rep/DECISIONS.md).
  given fetchingRegistry: (online: Online, loggable: Http.Event is Loggable, client: Http.Client)
  =>  (JsonPointer.Registry^{online, client}) =
    new JsonPointer.Registry:
      protected def lookup(url: HttpUrl): Optional[Json] =
        recover:
          case wisteria.Variant.Error(_, _, _) => Unset
          case Connect.Error(_)       => Unset
          case Http.Error(_, _)       => Unset
          case Parse.Error(_, _, _)   => Unset
          case Json.Error(_)          => Unset

        . protect(url.fetch().receive[Json])

extension (json: Json)
  // Runtime-checks `json` against the schema for `topic`, then re-types it as a
  // schema-typed `Json of topic from topic`. The phantom `Topic` records the
  // current position within the schema (initially the root, `topic`) and `Origin`
  // records the root schema itself, so that the `Dynamic` navigation macros can
  // resolve fields and array indices against `topic` at compile time — with no
  // `dynamicAccess.dynamicJson` import required.
  //
  // The conformance check walks `schematic.schema(): JsonSchema` against the
  // value, raising `Json.Error` on the first structural mismatch (wrong type, or a
  // required field absent).
  def verify[topic](using decodable: topic is Json.Decodable)
  :   (Json of topic from topic) raises Json.Error =

    conform(JsonSchema.reify(decodable.shape()), json.root)
    json.asInstanceOf[Json of topic from topic]


// Checks that a `Json.Ast` node conforms to a `JsonSchema`. An absent node is
// permitted only where the schema is optional; otherwise each variant checks the
// node's primitive type and recurses into object properties / array items.
private def conform(schema: JsonSchema, ast: Json.Ast): Unit raises Json.Error =
  inline def mismatch(expected: Json.Primitive): Unit =
    raise(Json.Error(Reason.NotType(ast.primitive, expected)))

  if ast.isAbsent then (if !schema.optional then raise(Json.Error(Reason.Absent))) else schema match
    case obj: JsonSchema.Object => obj.oneOf match
      case variants: List[JsonSchema] @scala.unchecked =>
        if
          !(variants: List[JsonSchema]).exists: variant =>
            safely(conform(variant, ast)).present
        then
          mismatch(Json.Primitive.Object)

      case _ =>
        if !ast.isObject then mismatch(Json.Primitive.Object) else
          val absent = Json.Ast(Unset)

          for (key, property) <- obj.properties do
            val index = ast.objectIndexOf(key.s)
            conform(property, if index < 0 then absent else ast.objectValue(index))

    case array: JsonSchema.Array =>
      if !ast.isArray then mismatch(Json.Primitive.Array)
      else array.items.lay(()): items =>
        var index = 0
        val length = ast.arrayLength

        while index < length do
          conform(items, ast.arrayElement(index))
          index += 1

    case _: JsonSchema.String  => if !ast.isString  then mismatch(Json.Primitive.String)
    case _: JsonSchema.Number  => if !ast.isNumber  then mismatch(Json.Primitive.Number)
    case _: JsonSchema.Integer => if !ast.isNumber  then mismatch(Json.Primitive.Number)
    case _: JsonSchema.Boolean => if !ast.isBoolean then mismatch(Json.Primitive.Boolean)
    case _: JsonSchema.Null    => if !ast.isNull    then mismatch(Json.Primitive.Null)

    // A `$ref` cannot be resolved without the root schema document, which is not
    // yet threaded through; such nodes are left unchecked for now.
    case _: JsonSchema.Ref => ()
