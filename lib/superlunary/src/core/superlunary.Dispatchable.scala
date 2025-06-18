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
┃    Soundness, version 0.34.0.                                                                    ┃
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
    type Carrier = Json
    type Format = Text

    inline def deserialize(text: Text): Array[Object] raises RemoteError =
      given RemoteError mitigates JsonError = error => RemoteError()
      Array.from(provide[Json is Decodable in Text](text.decode[Json].as[List[Json]]))

    inline def serialize(value: Array[Object]): Text = value.to(List).map(_.asInstanceOf[Json]).json.encode
    inline def embed[entity](value: entity): Json = provide[entity is Encodable in Json](value.json)

    inline def extract[entity](json: Json): entity raises RemoteError =
      given RemoteError mitigates JsonError = error => RemoteError()
      provide[entity is Decodable in Json](json.as[entity])

  given pojo: Dispatchable:
    type Carrier = Pojo
    type Format = Array[Pojo]

    inline def deserialize(value: Array[Pojo]): Array[Object] raises RemoteError = value.asInstanceOf[Array[Object]]
    inline def serialize(value: Array[Object]): Array[Pojo] = value.asInstanceOf[Array[Pojo]]

    inline def embed[entity](value: entity): Pojo =
      provide[entity is Encodable in Pojo](value.pojo)

    inline def extract[entity](pojo: Pojo): entity raises RemoteError =
      given RemoteError mitigates PojoError = error => RemoteError()
      provide[entity is Decodable in Pojo](pojo.as[entity])

trait Dispatchable:
  type Carrier <: Object
  type Format

  inline def embed[entity](value: entity): Carrier
  inline def serialize(values: Array[Object]): Format
  inline def deserialize(value: Format): Array[Object] raises RemoteError
  inline def extract[entity](value: Carrier): entity raises RemoteError
