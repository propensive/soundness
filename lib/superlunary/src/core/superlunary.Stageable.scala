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
package superlunary

import anticipation.*
import austronesian.*
import contingency.*
import distillate.*
import fulminate.*
import jacinta.*
import prepositional.*

import errorDiagnostics.stackTracesDiagnostics
import strategies.mitigation

object Stageable:
  given json: Stageable:
    type Transport = Json
    type Form = Text

    inline def deserialize(text: Text | Null): Array[Object] =
      provide[Tactic[RemoteError]]:
        given RemoteError mitigates JsonError =
          error => RemoteError(RemoteError.Reason.Deserialization)

        given RemoteError mitigates zephyrine.ParseError =
          error => RemoteError(RemoteError.Reason.Deserialization)

        // `Json.decodable` is named explicitly rather than left to `provide`'s deferred
        // search: this inline body expands inside staged programs, where the search is
        // sensitive to sibling expansions and can land on an inapplicable derivation.
        Array.from(text.nn.as[Json](using Json.decodable).as[List[Json]])

    inline def serialize(value: Array[Object]): Text =
      value.iterator.to(List).map(_.asInstanceOf[Json]).in[Json].encode

    inline def embed[entity](value: entity): Json = provide[entity is Encodable in Json](value.in[Json])

    inline def extract[entity](json: Json): entity = provide[Tactic[RemoteError]]:
      given RemoteError mitigates JsonError = error => RemoteError(RemoteError.Reason.Unknown)
      provide[entity is Decodable in Json](json.as[entity])

  given pojo: Stageable:
    type Transport = Pojo
    type Form = Array[Pojo]

    inline def deserialize(value: Array[Pojo] | Null): Array[Object] =
      value.asInstanceOf[Array[Object]]

    inline def serialize(value: Array[Object]): Array[Pojo] = value.asInstanceOf[Array[Pojo]]

    inline def embed[entity](value: entity): Pojo =
      infer[entity is Encodable in Pojo].encoded(value)

    inline def extract[entity](pojo: Pojo): entity =
      infer[entity is Decodable in Pojo].decoded(pojo)

trait Stageable extends Transportive, Formal:
  type Transport <: Object

  inline def embed[entity](value: entity): Transport
  inline def serialize(values: Array[Object]): Form
  inline def deserialize(value: Form | Null): Array[Object]
  inline def extract[entity](value: Transport): entity
