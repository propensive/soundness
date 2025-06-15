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
import jacinta.*
import prepositional.*
import rudiments.*

import interfaces.paths.pathOnLinux

import scala.quoted.*

object Dispatchable:
  given json: Dispatchable:
    type Carrier = Json
    type Format = Text

    def encoder[value: Type](using Quotes): Expr[value => Text] = '{_.json.encode}

    def decoder(using Quotes): Expr[Text => List[Json]] =
      '{ text => unsafely(text.decode[Json].as[List[Json]]) }

    inline def encode(value: List[Json]): Text = value.json.encode
    inline def decode[value](value: Text): value = unsafely(value.decode[Json].as[value])

    inline def embed[entity](value: entity): Json =
      compiletime.summonInline[entity is Encodable in Json].give(value.json)

    inline def extract[entity](json: Json): entity =
      compiletime.summonInline[Tactic[JsonError]].give:
        compiletime.summonInline[entity is Decodable in Json].give(json.as[entity])

  given pojo: Dispatchable:
    type Carrier = Pojo
    type Format = IArray[Pojo]

    def encoder[value: Type](using Quotes): Expr[value => IArray[Pojo]] = '{ value => value.pojo }

    def decoder(using Quotes): Expr[IArray[Pojo] => List[Pojo]] = '{_.to(List)}

    inline def encode(value: List[Pojo]): IArray[Pojo] = value.pojo

    inline def decode[value](value: Pojo): value =
      compiletime.summonInline[Tactic[PojoError]].give(value.as[value])

    inline def embed[entity](value: entity): Pojo =
      compiletime.summonInline[entity is Encodable in Pojo].give(value.pojo)

    inline def extract[entity](pojo: Pojo): entity =
      compiletime.summonInline[Tactic[PojoError]].give:
        compiletime.summonInline[entity is Decodable in Pojo].give(pojo.as[entity])

trait Dispatchable:
  type Carrier
  type Format

  def encoder[value: Type](using Quotes): Expr[value => Format]
  def decoder(using Quotes): Expr[Format => List[Carrier]]

  inline def encode(values: List[Carrier]): Format
  inline def decode[value](value: Format): value

  inline def embed[entity](value: entity): Carrier
  inline def extract[entity](value: Carrier): entity
