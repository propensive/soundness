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
package panopticon

import scala.caps

import anticipation.*
import denominative.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*
import denominative.dysasymptotics.{linearSize, linearAccess}

object Optical:
  given ordinalList: [element] => Ordinal is Optical from List[element] onto element =
    ordinal =>
      Optic: (origin, lambda) =>
        origin.at(ordinal).lay(origin): element =>
          origin.define(ordinal, lambda(element))

  given ordinalSeries: [element] => Ordinal is Optical from Sequence[element] onto element =
    ordinal =>
      Optic: (origin, lambda) =>
        origin.at(ordinal).lay(origin): element =>
          origin.define(ordinal, lambda(element))

  given at: [key, element] => key is Optical from Map[key, element] onto element =
    key =>
      Optic: (origin, lambda) =>
        origin(key).let(lambda).lay(origin)(value => origin.define(key, value))

  // The `predicate` laundering is for the Scala.js pipeline, which — unlike the JVM pipeline —
  // rejects the `Optic`'s capture of `filter.predicate` against the required pure `Optic` type.
  // (Compiler divergence; the JVM pipeline accepts the direct form.)
  given filter: [key, element] => Filter[key] is Optical from Map[key, element] onto element =
    filter =>
      val predicate: key -> Boolean = caps.unsafe.unsafeAssumePure(filter.predicate)

      Optic: (origin, lambda) =>
        // `Map.from`, not `.to[Map]`: the conversion route lets the (vacuous, strict-map)
        // `lambda` capture contaminate the result, where `from`'s signature is pure.
        Map.from:
          origin.stdlib.map: (key, value) =>
            if predicate(key) then (key, lambda(value)) else (key, value)

  given filter2: [element] => Filter[element] is Optical from List[element] onto element =
    filter =>
      val predicate: element -> Boolean = caps.unsafe.unsafeAssumePure(filter.predicate)

      Optic: (origin, lambda) =>
        origin.map: value => if predicate(value) then lambda(value) else value

trait Optical:
  type Self
  type Target
  type Origin

  def optic(self: Self): Optic from Origin onto Target
