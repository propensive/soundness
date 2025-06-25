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
┃    Soundness, version 0.36.0.                                                                    ┃
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
import distillate.{as as _, *}
import fulminate.*
import jacinta.*
import prepositional.*
import proscenium.*
import rudiments.*

import interfaces.paths.pathOnLinux
import errorDiagnostics.stackTraces
import strategies.mitigation

import scala.quoted.*

object Dispatchable:
  given json: Dispatchable:
    type Transport = Json
    type Form = Text

    inline def deserialize(text: Text | Null): Array[Object] =
      println(text)
      provide[Tactic[RemoteError]]:
        given RemoteError mitigates JsonError = error => RemoteError()
        Array.from(provide[Json is Decodable in Text](text.nn.decode[Json].as[List[Json]]))

    inline def serialize(value: Array[Object]): Text =
      value.to(List).map(_.asInstanceOf[Json]).json.encode

    inline def embed[entity](value: entity): Json = provide[entity is Encodable in Json](value.json)

    inline def extract[entity](json: Json): entity = provide[Tactic[RemoteError]]:
      given RemoteError mitigates JsonError = error => RemoteError()
      provide[entity is Decodable in Json](json.as[entity])

  given pojo: Dispatchable:
    type Transport = Pojo
    type Form = Array[Pojo]

    inline def deserialize(value: Array[Pojo] | Null): Array[Object] = value.asInstanceOf[Array[Object]]
    inline def serialize(value: Array[Object]): Array[Pojo] = value.asInstanceOf[Array[Pojo]]

    inline def embed[entity](value: entity): Pojo =
      infer[entity is Encodable in Pojo].encoded(value)

    inline def extract[entity](pojo: Pojo): entity =
      infer[entity is Decodable in Pojo].decoded(pojo)

trait Dispatchable extends Transportive, Formal:
  type Transport <: Object

  inline def embed[entity](value: entity): Transport
  inline def serialize(values: Array[Object]): Form
  inline def deserialize(value: Form | Null): Array[Object]
  inline def extract[entity](value: Transport): entity
