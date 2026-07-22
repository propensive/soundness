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
package xenophile

import anticipation.*
import prepositional.*
import vacuous.*

// The Kotlin/JVM ecosystem: `Interoperable` markers associating Scala types with the Kotlin
// types `KotlinDialect` resolves on demand from `@Metadata` in classfiles on the compile
// classpath. A nullable Kotlin type `T?` resolves as `T | "null"`, so `Optional` interop
// mirrors WIT's `option` (`T | "none"`) and TypeScript's `T | "undefined"`.
object Kotlin:
  // Instantiates a Kotlin class through its facade: `Kotlin.make[Pair[Text, Text]](t"a", t"b")`.
  inline def make[kotlinType](inline arguments: Any*): Facade over kotlinType =
    ${KotlinFacade.construct[kotlinType]('arguments)}

  // The facade of a class's companion object, through which its members are reachable:
  // `Kotlin.companion[Regex].escape(t"a.b")`.
  transparent inline def companion[kotlinType]: Any = ${KotlinFacade.companion[kotlinType]}

  given int: (Int is Interoperable in Kotlin of "kotlin.Int") =
    Interoperable[Int, Kotlin, "kotlin.Int"]()

  given long: (Long is Interoperable in Kotlin of "kotlin.Long") =
    Interoperable[Long, Kotlin, "kotlin.Long"]()

  given short: (Short is Interoperable in Kotlin of "kotlin.Short") =
    Interoperable[Short, Kotlin, "kotlin.Short"]()

  given byte: (Byte is Interoperable in Kotlin of "kotlin.Byte") =
    Interoperable[Byte, Kotlin, "kotlin.Byte"]()

  given boolean: (Boolean is Interoperable in Kotlin of "kotlin.Boolean") =
    Interoperable[Boolean, Kotlin, "kotlin.Boolean"]()

  given double: (Double is Interoperable in Kotlin of "kotlin.Double") =
    Interoperable[Double, Kotlin, "kotlin.Double"]()

  given float: (Float is Interoperable in Kotlin of "kotlin.Float") =
    Interoperable[Float, Kotlin, "kotlin.Float"]()

  given char: (Char is Interoperable in Kotlin of "kotlin.Char") =
    Interoperable[Char, Kotlin, "kotlin.Char"]()

  given text: (Text is Interoperable in Kotlin of "kotlin.String") =
    Interoperable[Text, Kotlin, "kotlin.String"]()

  given unit: (Unit is Interoperable in Kotlin of "kotlin.Unit") =
    Interoperable[Unit, Kotlin, "kotlin.Unit"]()

  // Kotlin `null` (produced by reading `T?` as `T | null`) is the absent `Optional` value.
  given none: (Unset.type is Interoperable in Kotlin of "null") =
    Interoperable[Unset.type, Kotlin, "null"]()

  // A nullable Kotlin type `T?` (read as `T | null`) corresponds to a Scala `Optional`. The
  // `Mandatable` constraint identifies the mandatory type `inner`, so the instance applies only
  // to genuine optionals and never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( inner is Interoperable in Kotlin of topic )
  =>  ( value is Interoperable in Kotlin of (topic | "null") ) =
    Interoperable[value, Kotlin, (topic | "null")]()

trait Kotlin extends Ecosystem:
  type Grammar = KotlinDialect.type
