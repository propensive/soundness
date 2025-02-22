<<<<<<< HEAD
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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package austronesian

import scala.collection.Factory

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import rudiments.*
import wisteria.*

object Austronesian:
  opaque type Stdlib =
    IArray[Any] | String | Boolean | Byte | Char | Short | Int | Long | Float | Double

  object Stdlib extends Stdlib2:
    given text: Text is Encodable in Stdlib = _.s
    given string: String is Encodable in Stdlib = identity(_)
    given int: Int is Encodable in Stdlib = identity(_)
    given long: Long is Encodable in Stdlib = identity(_)
    given float: Float is Encodable in Stdlib = identity(_)
    given double: Double is Encodable in Stdlib = identity(_)
    given char: Char is Encodable in Stdlib = identity(_)
    given boolean: Boolean is Encodable in Stdlib = identity(_)
    given byte: Byte is Encodable in Stdlib = identity(_)

    given list: [CollectionType <: Iterable, ElementType: Encodable in Stdlib]
    =>     CollectionType[ElementType] is Encodable in Stdlib =
      iterable => IArray.from(iterable.map(_.encode))

    given text2: Tactic[StdlibError] => Text is Decodable in Stdlib =
      case string: String => string.tt
      case _              => raise(StdlibError()) yet "".tt

    given string2: Tactic[StdlibError] => String is Decodable in Stdlib =
      case string: String => string
      case _              => raise(StdlibError()) yet ""

    given int2: Tactic[StdlibError] => Int is Decodable in Stdlib =
      case int: Int => int
      case _        => raise(StdlibError()) yet 0

    given long2: Tactic[StdlibError] => Long is Decodable in Stdlib =
      case long: Long => long
      case _          => raise(StdlibError()) yet 0L

    given float2: Tactic[StdlibError] => Float is Decodable in Stdlib =
      case float: Float => float
      case _            => raise(StdlibError()) yet 0.0f

    given double2: Tactic[StdlibError] => Double is Decodable in Stdlib =
      case double: Double => double
      case _              => raise(StdlibError()) yet 0.0

    given char2: Tactic[StdlibError] => Char is Decodable in Stdlib =
      case char: Char => char
      case _          => raise(StdlibError()) yet '\u0000'

    given boolean2: Tactic[StdlibError] => Boolean is Decodable in Stdlib =
      case boolean: Boolean => boolean
      case _                => raise(StdlibError()) yet false

    given collection: [CollectionType <: Iterable, ElementType: Decodable in Stdlib]
    =>    Tactic[StdlibError]
    =>    (factory: Factory[ElementType, CollectionType[ElementType]])
    =>    CollectionType[ElementType] is Decodable in Stdlib =

      case array: Array[Stdlib] =>
        factory.newBuilder.pipe: builder =>
          array.each(builder += _.decode)
          builder.result()

      case other =>
        raise(StdlibError()) yet factory.newBuilder.result()

  trait Stdlib2:
    inline given encodable: [ValueType: Reflection] => ValueType is Encodable in Stdlib =
      Austronesian2.EncodableDerivation.derived

    inline given decodable: [ValueType: Reflection] => ValueType is Decodable in Stdlib =
      Austronesian2.DecodableDerivation.derived
