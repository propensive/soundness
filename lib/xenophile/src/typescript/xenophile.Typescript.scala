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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import prepositional.*
import vacuous.*

// The TypeScript ecosystem: a set of `Interoperable` markers associating Scala types with the
// foreign types `TypescriptDialect` reads from `.ts` definition files. No runtime representation is
// involved — these only record the type correspondence used for type-checking and conversion.
object Typescript:
  given text: (Text is Interoperable in Typescript of "string") =
    Interoperable[Text, Typescript, "string"]()

  given int: (Int is Interoperable in Typescript of "number") =
    Interoperable[Int, Typescript, "number"]()

  given boolean: (Boolean is Interoperable in Typescript of "boolean") =
    Interoperable[Boolean, Typescript, "boolean"]()

  // A TypeScript `T[]` (i.e. `Array<T>`) corresponds to a Scala `List` of the element type.
  given list: [element, topic]
  =>  ( element is Interoperable in Typescript of topic )
  =>  ( List[element] is Interoperable in Typescript of ("Array" over topic) ) =
    Interoperable[List[element], Typescript, ("Array" over topic)]()

  // A TypeScript `Map<K, V>` corresponds to a Scala `Map`.
  given map: [key, value, keyTopic, valueTopic]
  =>  ( key is Interoperable in Typescript of keyTopic,
        value is Interoperable in Typescript of valueTopic )
  =>  ( Map[key, value] is Interoperable in Typescript of ("Map" over (keyTopic, valueTopic)) ) =
    Interoperable[Map[key, value], Typescript, ("Map" over (keyTopic, valueTopic))]()

  // TypeScript `undefined` (produced by reading `T?` as `T | undefined`) is the absent `Optional`.
  given undefined: (Unset.type is Interoperable in Typescript of "undefined") =
    Interoperable[Unset.type, Typescript, "undefined"]()

  // A TypeScript `T?` (read as `T | undefined`) corresponds to a Scala `Optional`. The `Mandatable`
  // constraint identifies the mandatory type `inner`, so the instance applies only to genuine
  // optionals and never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( inner is Interoperable in Typescript of topic )
  =>  ( value is Interoperable in Typescript of (topic | "undefined") ) =
    Interoperable[value, Typescript, (topic | "undefined")]()

trait Typescript extends Ecosystem:
  type Grammar = TypescriptDialect.type
